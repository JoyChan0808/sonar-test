package com.aam.producer.playlist.biz.service.domain.impl;

import com.aam.authentication.acl.exception.AuthException;
import com.aam.authentication.acl.utils.UserInfoUtil;
import com.aam.producer.lib.enums.ContentTypeEnum;
import com.aam.producer.playlist.biz.enums.ResultCodeEnum;
import com.aam.producer.playlist.biz.enums.SegmentSplitTypeEnum;
import com.aam.producer.playlist.biz.enums.SegmentStatusEnum;
import com.aam.producer.playlist.biz.service.IPlaylistSegmentSplitAssociationService;
import com.aam.producer.playlist.biz.service.IPlaylistService;
import com.aam.producer.playlist.biz.service.IPlaylistVersionContentAssociationService;
import com.aam.producer.playlist.biz.service.IPosPlaylistMappingService;
import com.aam.producer.playlist.biz.service.ISegmentService;
import com.aam.producer.playlist.biz.service.ISegmentSplitService;
import com.aam.producer.playlist.biz.service.domain.IPlaylistShowAttributeViewService;
import com.aam.producer.playlist.biz.service.domain.IPlaylistVersionActionService;
import com.aam.producer.playlist.biz.service.domain.IPlaylistVersionContentViewService;
import com.aam.producer.playlist.biz.service.domain.IPlaylistVersionViewService;
import com.aam.producer.playlist.biz.service.domain.ISegmentSplitActionService;
import com.aam.producer.playlist.biz.service.domain.ISegmentSplitContentViewService;
import com.aam.producer.playlist.biz.service.domain.ISegmentSplitViewService;
import com.aam.producer.playlist.biz.util.OrgUtil;
import com.aam.producer.playlist.protocol.request.SegmentSplitDTO;
import com.aam.producer.playlist.protocol.response.ContentInfo;
import com.aam.producer.playlist.protocol.response.PlaylistVersionInfo;
import com.aam.producer.playlist.protocol.response.SegmentSplitInfo;
import com.aam.producer.playlist.protocol.response.ShowAttributeGroupInfo;
import com.aam.producer.playlist.protocol.response.SplitComplexInfo;
import com.aam.producer.playlist.protocol.response.SplitComplexInfo1;
import com.aam.producer.playlist.protocol.response.TitleInfo;
import com.aam.producer.playlist.repository.entity.PlaylistDO;
import com.aam.producer.playlist.repository.entity.PlaylistSegmentSplitAssociationDO;
import com.aam.producer.playlist.repository.entity.PlaylistVersionContentAssociationDO;
import com.aam.producer.playlist.repository.entity.PosPlaylistMappingDO;
import com.aam.producer.playlist.repository.entity.SegmentDO;
import com.aam.producer.playlist.repository.entity.SegmentSplitDO;
import com.aam.producer.playlist.sal.client.IComplexFacadeClient;
import com.aam.producer.playlist.sal.client.ITitleFacadeClient;
import com.aam.producer.playlist.sal.response.Complex;
import com.aam.producer.playlist.sal.response.ComplexGroup;
import com.aam.producer.playlist.sal.response.PosInfo;
import com.aam.utils.enums.BaseResultCode;
import com.aam.utils.exception.BizException;
import com.aam.utils.utils.SpringContextUtils;
import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.TypeReference;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.Stack;
import java.util.stream.Collectors;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
public class SegmentSplitViewServiceImpl implements ISegmentSplitViewService {

    private final static Logger logger = LoggerFactory.getLogger(SegmentSplitViewServiceImpl.class);
    private final ISegmentSplitService iSegmentSplitService;
    private final ISegmentService iSegmentService;
    private final IPlaylistSegmentSplitAssociationService segmentSplitAssociationService;
    private final ISegmentSplitContentViewService segmentContentViewService;
    private final IPosPlaylistMappingService iPosPlaylistMappingService;
    private final IPlaylistVersionContentAssociationService contentAssociationService;
    private final IComplexFacadeClient iComplexFacadeClient;
    private final IPlaylistShowAttributeViewService iPlaylistShowAttributeViewService;
    private final ITitleFacadeClient iTitleFacadeClient;
    private final IPlaylistService iPlaylistService;

    @Autowired
    public SegmentSplitViewServiceImpl(ISegmentSplitService iSegmentSplitService,
            IPlaylistSegmentSplitAssociationService segmentSplitAssociationService,
            ISegmentSplitContentViewService segmentContentViewService,
            IPosPlaylistMappingService iPosPlaylistMappingService,
            IPlaylistVersionContentAssociationService contentAssociationService,
            IComplexFacadeClient iComplexFacadeClient,
            IPlaylistShowAttributeViewService iPlaylistShowAttributeViewService,
            ITitleFacadeClient iTitleFacadeClient,
            ISegmentService iSegmentService,
            IPlaylistService iPlaylistService) {
        this.iSegmentSplitService = iSegmentSplitService;
        this.segmentSplitAssociationService = segmentSplitAssociationService;
        this.segmentContentViewService = segmentContentViewService;
        this.iPosPlaylistMappingService = iPosPlaylistMappingService;
        this.contentAssociationService = contentAssociationService;
        this.iComplexFacadeClient = iComplexFacadeClient;
        this.iPlaylistShowAttributeViewService = iPlaylistShowAttributeViewService;
        this.iTitleFacadeClient = iTitleFacadeClient;
        this.iSegmentService = iSegmentService;
        this.iPlaylistService = iPlaylistService;
    }

    @Override
    public List<SegmentSplitInfo> getByContentAssociationUuid(
            String contentAssociationUuid,
            String titleUuid) {

        List<SegmentSplitInfo> segmentSplitInfoList = getDraftByContentAssociationUuid(
                contentAssociationUuid, titleUuid);

        segmentSplitInfoList
                .addAll(getReleaseByContentAssociationUuid(contentAssociationUuid, titleUuid));
        return segmentSplitInfoList;
    }

    @Override
    public List<SegmentSplitInfo> getDraftByContentAssociationUuid(
            String contentAssociationUuid,
            String titleUuid) {

        ISegmentSplitActionService iSegmentSplitActionService = SpringContextUtils
                .getBean(ISegmentSplitActionService.class);
        iSegmentSplitActionService.createDefaultSegmentSplit(contentAssociationUuid, titleUuid);

        List<SegmentSplitInfo> segmentSplitInfoList = getSegmentSplitList(contentAssociationUuid,
                titleUuid, SegmentStatusEnum.DRAFT.getStatus());

        if (segmentSplitInfoList.isEmpty()) {
            return segmentSplitInfoList;
        }

        boolean isBaseSegment = StringUtils.isEmpty(segmentSplitInfoList.get(0).getPlaylistUuid());
        List<PosInfo> posInfos;
        if (isBaseSegment) {
            posInfos = getPosInfos(contentAssociationUuid);
        } else {
            posInfos = getPosInfos(contentAssociationUuid, titleUuid);
        }
        List<SegmentSplitInfo> segmentSplitInfosTree = buildSplitRuleTree(segmentSplitInfoList);

        fillDetailInfo(segmentSplitInfosTree, posInfos);
        List<SegmentSplitInfo> others = segmentSplitInfosTree.stream().flatMap(
                segmentSplitInfo -> addOtherSegmentSplitInfos(segmentSplitInfo, posInfos).stream())
                .collect(Collectors.toList());
        segmentSplitInfoList.addAll(others);

        segmentSplitInfosTree.stream().collect(Collectors.groupingBy(SegmentSplitInfo::getStatus))
                .forEach((s, segmentSplitInfos) -> {
                    //统计
                    count(segmentSplitInfos, posInfos.size());
                });

        return segmentSplitInfoList;
    }

    private List<PosInfo> getPosInfos(String contentAssociationUuid, String titleUuid) {
        IPlaylistVersionActionService iPlaylistVersionService = SpringContextUtils
                .getBean(IPlaylistVersionActionService.class);

        String playlistUuid = getPplUuid(contentAssociationUuid, titleUuid);
        PlaylistDO playlistDO = iPlaylistService.getPlaylist(playlistUuid);
        List<PosInfo> posInfos = playlistDO.getAutomaticallyApply() ?
                iPlaylistVersionService.toPosInfo(
                        iPosPlaylistMappingService.getMappingsByShowAttribute(
                                OrgUtil.orgContenter.get(),
                                false,
                                titleUuid,
                                getShowTypeCodes(contentAssociationUuid, titleUuid)))
                : new ArrayList<>();

        iPlaylistVersionService.setPosFilmHallInfos(posInfos);

        return posInfos;
    }

    private List<PosInfo> getPosInfos(String segmentUuid) {
        IPlaylistVersionActionService iPlaylistVersionService = SpringContextUtils
                .getBean(IPlaylistVersionActionService.class);
        IPlaylistVersionContentViewService contentViewService = SpringContextUtils
                .getBean(IPlaylistVersionContentViewService.class);
        List<PlaylistVersionContentAssociationDO> associationDOS = contentViewService
                .getByContentId(segmentUuid);
        List<Long> showTypeGroup = associationDOS.stream().map(
                PlaylistVersionContentAssociationDO::getPplVersionId).distinct().flatMap(
                pplVersionUuid -> iPlaylistShowAttributeViewService
                        .getCodesByPplVersionUuid(pplVersionUuid).stream()).distinct()
                .collect(Collectors.toList());
        List<PosInfo> posInfos = CollectionUtils.isNotEmpty(showTypeGroup) ?
                iPlaylistVersionService.toPosInfo(
                        iPosPlaylistMappingService.getMappingsByShowAttribute(
                                OrgUtil.orgContenter.get(),
                                false,
                                null,
                                showTypeGroup))
                : new ArrayList<>();
        iPlaylistVersionService.setPosFilmHallInfos(posInfos);
        return posInfos;
    }

    private List<SegmentSplitInfo> addOtherSegmentSplitInfos(SegmentSplitInfo rootSegmentSplitInfo,
            List<PosInfo> posInfos) {
        List<SegmentSplitInfo> list = new ArrayList<>();
        Stack<SegmentSplitInfo> stack = new Stack<>();
        stack.push(rootSegmentSplitInfo);
        while (!stack.isEmpty()) {
            SegmentSplitInfo segmentSplitInfo = stack.pop();
            if (!segmentSplitInfo.getChildren().isEmpty()) {
                int i = segmentSplitInfo.getShows() - segmentSplitInfo.getChildren().stream()
                        .mapToInt(SegmentSplitInfo::getShows).sum();
                if (i != 0) {
                    SegmentSplitDTO segmentSplitDTO = new SegmentSplitDTO();
                    segmentSplitDTO.setAutoGroup(true);
                    segmentSplitDTO.setStatus(SegmentStatusEnum.DRAFT.getStatusStr());
                    segmentSplitDTO.setPlaylistUuid(segmentSplitInfo.getPlaylistUuid());
                    segmentSplitDTO.setPplVersionUuid(segmentSplitInfo.getPplVersionUuid());
                    segmentSplitDTO.setSplitTitle("other");
                    segmentSplitDTO.setSplitType(SegmentSplitTypeEnum.SHOWS.getName());
                    segmentSplitDTO.setTitleUuid(segmentSplitInfo.getTitleUuid());
                    List<String> posUuids = segmentSplitInfo.getShowUuids().stream()
                            .filter(s -> !segmentSplitInfo.getChildren().stream()
                                    .flatMap(segmentSplitInfo1 -> segmentSplitInfo1.getShowUuids()
                                            .stream())
                                    .collect(Collectors.toList()).contains(s))
                            .collect(Collectors.toList());
                    segmentSplitDTO.setSplitRule(JSON.toJSONString(posUuids));
                    segmentSplitDTO.setDefaultGroup(false);
                    segmentSplitDTO.setUserGroup(false);
                    segmentSplitDTO.setSegmentAssociationUuid(
                            segmentSplitInfo.getSegmentAssociationUuid());
                    segmentSplitDTO.setParentUuid(segmentSplitInfo.getUuid());

                    ISegmentSplitActionService iSegmentSplitActionService = SpringContextUtils
                            .getBean(ISegmentSplitActionService.class);

                    String uuid = iSegmentSplitActionService
                            .createOrUpdateSegmentSplit(segmentSplitDTO);
                    SegmentSplitDO segmentSplitDO = iSegmentSplitService.getById(uuid);

                    SegmentSplitInfo other = toSegmentSplitInfo(segmentSplitDO,
                            segmentSplitDTO.getSegmentAssociationUuid()
                            , segmentSplitDTO.getPlaylistUuid(),
                            segmentSplitDTO.getPplVersionUuid(),
                            segmentSplitDTO.getTitleUuid(), new ArrayList<>());
                    other.setShows(i);
                    other.setSites(posInfos.stream()
                            .filter(posInfo -> posUuids.contains(posInfo.getPosUuid()))
                            .map(PosInfo::getComplexId)
                            .collect(Collectors.toSet()).size());
                    list.add(other);
                    segmentSplitInfo.getChildren().add(other);
                }
                segmentSplitInfo.getChildren().forEach(stack::push);
            }
        }
        return list;
    }

    private List<Long> getShowTypeCodes(String contentAssociationUuid, String titleUuid) {
        List<PlaylistSegmentSplitAssociationDO> splitAssociationDOS = segmentSplitAssociationService
                .getSegmentSplitByContentAssociationUuid(contentAssociationUuid, titleUuid);
        if (splitAssociationDOS.isEmpty()) {
            return new ArrayList<>();
        }
        String versionId = splitAssociationDOS.get(0).getPplVersionId();
        return iPlaylistShowAttributeViewService.getCodesByPplVersionUuid(versionId);
    }

    private List<SegmentSplitInfo> getReleaseByContentAssociationUuid(
            String contentAssociationUuid,
            String titleUuid) {
        List<SegmentSplitInfo> segmentSplitInfoList = getSegmentSplitList(contentAssociationUuid,
                titleUuid, SegmentStatusEnum.RELEASE.getStatus());

        if (segmentSplitInfoList.isEmpty()) {
            return segmentSplitInfoList;
        }

        boolean isBaseSegment = StringUtils.isEmpty(segmentSplitInfoList.get(0).getPlaylistUuid());
        List<PosInfo> posInfos;
        if (isBaseSegment) {
            IPlaylistVersionContentViewService contentViewService = SpringContextUtils
                    .getBean(IPlaylistVersionContentViewService.class);
            List<PlaylistVersionContentAssociationDO> associationDOS = contentViewService
                    .getByContentId(contentAssociationUuid);
            posInfos = associationDOS.stream()
                    .map(PlaylistVersionContentAssociationDO::getPlaylistUuid).distinct()
                    .flatMap(playlistUuid -> getPosInfo(playlistUuid,
                            null).stream()).collect(Collectors.toList());
        } else {
            posInfos = getPosInfo(getPplUuid(contentAssociationUuid, titleUuid),
                    titleUuid);
        }

        List<SegmentSplitInfo> segmentSplitInfosTree = buildSplitRuleTree(segmentSplitInfoList);
        fillDetailInfo(segmentSplitInfosTree, posInfos);
        segmentSplitInfosTree.stream().collect(Collectors.groupingBy(SegmentSplitInfo::getStatus))
                .forEach((s, segmentSplitInfos) -> {
                    //统计
                    count(segmentSplitInfos, posInfos.size());
                });

        return segmentSplitInfoList;
    }

    private void fillDetailInfo(List<SegmentSplitInfo> segmentSplitInfosTree,
            List<PosInfo> posInfos) {
        try {
            List<SplitComplexInfo> splitComplexInfos = iComplexFacadeClient
                    .getComplex(UserInfoUtil.getGroups());

            fillComplex(segmentSplitInfosTree,
                    splitComplexInfos);

            IPlaylistVersionViewService iPlaylistVersionViewService = SpringContextUtils
                    .getBean(IPlaylistVersionViewService.class);
            PlaylistVersionInfo playlistVersionInfo = iPlaylistVersionViewService
                    .getPlaylistVersion(segmentSplitInfosTree.get(0).getPplVersionUuid());
            fillShowType(segmentSplitInfosTree, playlistVersionInfo.getShowAttributeGroups());
        } catch (AuthException e) {
            //不做处理
        } catch (Exception e) {
            logger.error("Fill detail info exception.", e);
        }

        if (posInfos.isEmpty()) {
            return;
        }

        //匹配场次
        segmentSplitInfosTree.stream().collect(Collectors.groupingBy(SegmentSplitInfo::getStatus))
                .forEach((s, segmentSplitInfos) -> matchPos(segmentSplitInfos, posInfos));

    }

    private String getPplUuid(String contentAssociationUuid, String titleUuid) {
        List<PlaylistSegmentSplitAssociationDO> splitAssociationDOS = segmentSplitAssociationService
                .getSegmentSplitByContentAssociationUuid(contentAssociationUuid, titleUuid);
        return splitAssociationDOS.get(0).getPlaylistUuid();
    }

    private List<PosInfo> getPosInfo(String pplUuid, String titleUuid) {
        IPlaylistVersionActionService iPlaylistVersionService = SpringContextUtils
                .getBean(IPlaylistVersionActionService.class);
        List<PosPlaylistMappingDO> posPlaylistMappingDOS;
        if (StringUtils.isNotBlank(titleUuid)) {
            posPlaylistMappingDOS = iPosPlaylistMappingService
                    .getMappingsByPplFilterTitle(false, pplUuid, titleUuid);
        } else {
            posPlaylistMappingDOS = iPosPlaylistMappingService
                    .getMappingsByPpl(false, pplUuid);
        }
        List<PosInfo> posInfos = iPlaylistVersionService.toPosInfo(posPlaylistMappingDOS);
        iPlaylistVersionService.setPosFilmHallInfos(posInfos);
        return posInfos;
    }

    private void matchPos(List<SegmentSplitInfo> segmentSplitInfosTree, List<PosInfo> posInfos) {
        IPlaylistVersionViewService iPlaylistVersionService = SpringContextUtils
                .getBean(IPlaylistVersionViewService.class);
        segmentSplitInfosTree.forEach(segmentSplitInfo -> iPlaylistVersionService
                .segmentSplitMatchPos(segmentSplitInfo, posInfos));
    }

    private void count(List<SegmentSplitInfo> segmentSplitInfos, int totals) {
        Stack<SegmentSplitInfo> stack = new Stack<>();
        SegmentSplitInfo root = new SegmentSplitInfo();
        root.setChildren(segmentSplitInfos);
        stack.push(root);
        List<SegmentSplitInfo> leaf = new ArrayList<>();
        while (!stack.isEmpty()) {
            SegmentSplitInfo curr = stack.pop();
            List<SegmentSplitInfo> children = curr.getChildren();
            if (CollectionUtils.isEmpty(children)) {
                leaf.add(curr);
            } else {
                children.forEach(stack::push);
            }
        }

        if (leaf.size() > 1) {
            List<SegmentSplitInfo> tailList = leaf.subList(1, leaf.size());
            List<SegmentSplitInfo> headList = leaf.subList(0, 1);

            tailList.forEach(segmentSplitInfo -> {
                if (totals != 0) {
                    segmentSplitInfo.setPercentage((segmentSplitInfo.getShows() * 100) / totals);
                } else {
                    segmentSplitInfo.setPercentage(0);
                }
                int duration = calculationDuration(segmentSplitInfo);
                segmentSplitInfo.setDuration(duration);
            });

            headList.forEach(segmentSplitInfo -> {
                if (totals != 0) {
                    segmentSplitInfo.setPercentage(
                            (100 - tailList.stream().mapToInt(SegmentSplitInfo::getPercentage)
                                    .sum()));
                } else {
                    segmentSplitInfo.setPercentage(0);
                }
                int duration = calculationDuration(segmentSplitInfo);
                segmentSplitInfo.setDuration(duration);
            });

        } else {
            leaf.forEach(segmentSplitInfo -> {
                if (totals != 0) {
                    segmentSplitInfo.setPercentage(100);
                } else {
                    segmentSplitInfo.setPercentage(0);
                }
                int duration = calculationDuration(segmentSplitInfo);
                segmentSplitInfo.setDuration(duration);
            });
        }
    }

    private int calculationDuration(SegmentSplitInfo segmentSplitInfo) {
        return segmentSplitInfo.getContentList().stream()
                .filter(contentInfo -> ContentTypeEnum.CPL.getName()
                        .equals(contentInfo.getContentType()))
                .map(contentInfo -> JSON.parseObject(contentInfo.getExtension()))
                .map(jsonObject -> jsonObject.getInteger("duration_in_frames") /
                        jsonObject.getInteger("duration_in_seconds"))
                .mapToInt(durationNumerator -> durationNumerator).sum();
    }

    @Override
    public List<SplitComplexInfo1> getSplitComplex(String uuid, List<String> complexGroupIds) {
        PlaylistSegmentSplitAssociationDO playlistSegmentSplitAssociationDO = segmentSplitAssociationService
                .getSegmentSplitBySegmentSplitUuid(uuid);
        List<SegmentSplitInfo> segmentSplitInfos = getSegmentSplitList(
                playlistSegmentSplitAssociationDO.getSegmentAssociationUuid(),
                playlistSegmentSplitAssociationDO.getTitleId(), null);
        List<SegmentSplitInfo> treeList = buildSplitRuleTree(segmentSplitInfos);
        List<SplitComplexInfo> splitComplexInfos = iComplexFacadeClient.getComplex(complexGroupIds);
        fillComplex(treeList, splitComplexInfos);
        return mapComplexGroup(findSegmentSplitInfo(segmentSplitInfos, uuid).getComplexList());
    }

    private List<SplitComplexInfo1> mapComplexGroup(List<SplitComplexInfo> splitComplexInfos) {
        List<ComplexGroup> complexGroups = UserInfoUtil.getGroups().parallelStream().map(
                iComplexFacadeClient::getComplexGroup)
                .filter(complexGroup -> complexGroup.getComplexes().stream()
                        .map(Complex::getUuid)
                        .anyMatch(s -> splitComplexInfos.stream().map(SplitComplexInfo::getUuid)
                                .anyMatch(s1 -> s1.equals(s)))).collect(
                        Collectors.toList());

        return complexGroups.stream()
                .flatMap(complexGroup -> complexGroup.getComplexes().stream())
                .map(splitComplexInfo -> {
                    SplitComplexInfo1 splitComplexInfo1 = new SplitComplexInfo1();
                    splitComplexInfo1.setUuid(splitComplexInfo.getUuid());
                    splitComplexInfo1.setName(splitComplexInfo.getName());
                    splitComplexInfo1.setEnable(splitComplexInfos.stream().anyMatch(
                            splitComplexInfo2 -> splitComplexInfo2.getUuid()
                                    .equals(splitComplexInfo.getUuid())));
                    splitComplexInfo1.setGroupNames(complexGroups.stream()
                            .filter(complexGroup -> complexGroup.getComplexes().stream()
                                    .map(Complex::getUuid)
                                    .anyMatch(s -> s.equals(splitComplexInfo.getUuid())))
                            .map(ComplexGroup::getName).distinct().collect(Collectors.toList()));
                    return splitComplexInfo1;
                }).distinct().collect(Collectors.toList());
    }

    private SegmentSplitInfo findSegmentSplitInfo(List<SegmentSplitInfo> segmentSplitInfos,
            String segmentSplitUuid) {
        for (SegmentSplitInfo segmentSplitInfo : segmentSplitInfos) {
            if (segmentSplitInfo.getUuid().equals(segmentSplitUuid)) {
                return segmentSplitInfo;
            }
        }
        throw new BizException(BaseResultCode.SYSTEM_ERROR);
    }

    private void fillComplex(List<SegmentSplitInfo> segmentSplitInfos,
            List<SplitComplexInfo> splitComplexInfos) {
        List<SegmentSplitInfo> otherSegmentSplit = segmentSplitInfos.stream()
                .filter(segmentSplitInfo -> !Boolean.TRUE
                        .equals(segmentSplitInfo.getDefaultGroup()))
                .collect(Collectors.toList());
        otherSegmentSplit.forEach(segmentSplitInfo -> {
            if (SegmentSplitTypeEnum.SITES.getName().equals(segmentSplitInfo.getSplitType())) {
                List<String> complexes = JSON
                        .parseArray(segmentSplitInfo.getSplitRule(), String.class);
                segmentSplitInfo.setComplexList(splitComplexInfos.stream()
                        .filter(splitComplexInfo -> complexes.contains(splitComplexInfo.getUuid()))
                        .collect(Collectors.toList()));
            } else {
                segmentSplitInfo.setComplexList(splitComplexInfos);
            }
            segmentSplitInfo.setRuleSites(segmentSplitInfo.getComplexList().size());
            if (!segmentSplitInfo.getChildren().isEmpty()) {
                fillComplex(segmentSplitInfo.getChildren(), segmentSplitInfo.getComplexList());
            }
        });
        segmentSplitInfos.stream()
                .filter(segmentSplitInfo -> Boolean.TRUE.equals(segmentSplitInfo.getDefaultGroup()))
                .findFirst().
                ifPresent(defaultSegmentSplit -> {
                    if (SegmentSplitTypeEnum.SITES.getName()
                            .equals(defaultSegmentSplit.getSplitType())) {
                        defaultSegmentSplit.setComplexList(splitComplexInfos.stream().filter(
                                s -> otherSegmentSplit.stream()
                                        .flatMap(segmentSplitInfo -> segmentSplitInfo
                                                .getComplexList().stream())
                                        .noneMatch(s1 -> Objects.equals(s, s1)))
                                .collect(Collectors.toList()));
                        defaultSegmentSplit.setSplitRule(
                                JSON.toJSONString(defaultSegmentSplit.getComplexList().stream().map(
                                        SplitComplexInfo::getUuid).collect(
                                        Collectors.toList())));
                    } else {
                        defaultSegmentSplit.setComplexList(splitComplexInfos);
                    }
                    defaultSegmentSplit.setRuleSites(defaultSegmentSplit.getComplexList().size());
                    if (!defaultSegmentSplit.getChildren().isEmpty()) {
                        fillComplex(defaultSegmentSplit.getChildren(),
                                defaultSegmentSplit.getComplexList());
                    }
                });
    }


    private void fillShowType(List<SegmentSplitInfo> segmentSplitInfos,
            List<ShowAttributeGroupInfo> showAttributeGroupInfos) {

        List<SegmentSplitInfo> otherSegmentSplit = segmentSplitInfos.stream()
                .filter(segmentSplitInfo -> !Boolean.TRUE
                        .equals(segmentSplitInfo.getDefaultGroup()))
                .collect(Collectors.toList());

        otherSegmentSplit.forEach(segmentSplitInfo -> {
            if (SegmentSplitTypeEnum.SHOW_ATTR.getName().equals(segmentSplitInfo.getSplitType())) {
                List<List<String>> showTypes;
                try {
                    //Compatible with old formats.
                    showTypes = JSON.parseObject(segmentSplitInfo.getSplitRule(),
                            new TypeReference<List<List<String>>>() {
                            });
                } catch (Exception e) {
                    List<ShowAttributeGroupInfo> list = JSON
                            .parseObject(segmentSplitInfo.getSplitRule(),
                                    new TypeReference<List<ShowAttributeGroupInfo>>() {
                                    });
                    showTypes = list.stream().map(
                            ShowAttributeGroupInfo::getAttributes).collect(
                            Collectors.toList());
                }

                List<List<String>> finalShowTypes = showTypes;
                segmentSplitInfo.setShowAttributeGroupInfos(showAttributeGroupInfos.stream()
                        .filter(showAttributeGroupInfo -> {
                            List<String> showTypeList = finalShowTypes.stream()
                                    .map(strings -> strings.stream().sorted()
                                            .collect(Collectors.joining(",")))
                                    .collect(Collectors.toList());
                            String showType = showAttributeGroupInfo.getAttributes().stream()
                                    .sorted().collect(Collectors.joining(","));
                            return showTypeList.contains(showType);
                        }).collect(Collectors.toList()));
            } else {
                segmentSplitInfo.setShowAttributeGroupInfos(showAttributeGroupInfos);
            }

            if (!segmentSplitInfo.getChildren().isEmpty()) {
                fillShowType(segmentSplitInfo.getChildren(),
                        segmentSplitInfo.getShowAttributeGroupInfos());
            }
        });

        segmentSplitInfos.stream()
                .filter(segmentSplitInfo -> Boolean.TRUE.equals(segmentSplitInfo.getDefaultGroup()))
                .findFirst().
                ifPresent(defaultSegmentSplit -> {
                    if (SegmentSplitTypeEnum.SHOW_ATTR.getName()
                            .equals(defaultSegmentSplit.getSplitType())) {
                        defaultSegmentSplit
                                .setShowAttributeGroupInfos(showAttributeGroupInfos.stream().filter(
                                        s -> otherSegmentSplit.stream()
                                                .flatMap(segmentSplitInfo -> segmentSplitInfo
                                                        .getShowAttributeGroupInfos().stream())
                                                .noneMatch(s1 -> Objects.equals(s, s1)))
                                        .collect(Collectors.toList()));
                        defaultSegmentSplit.setSplitRule(
                                JSON.toJSONString(
                                        defaultSegmentSplit.getShowAttributeGroupInfos().stream()
                                                .map(
                                                        ShowAttributeGroupInfo::getAttributes)
                                                .collect(
                                                        Collectors.toList())));
                    } else {
                        defaultSegmentSplit.setShowAttributeGroupInfos(showAttributeGroupInfos);
                    }
                    if (!defaultSegmentSplit.getChildren().isEmpty()) {
                        fillShowType(defaultSegmentSplit.getChildren(),
                                defaultSegmentSplit.getShowAttributeGroupInfos());
                    }
                });
    }

    @Override
    public List<SegmentSplitInfo> getSegmentSplitList(String associationUuid, String titleUuid,
            Integer status) {
        List<PlaylistSegmentSplitAssociationDO> splitAssociationDOS = segmentSplitAssociationService
                .getSegmentSplitByContentAssociationUuid(associationUuid, titleUuid);
        if (splitAssociationDOS.isEmpty()) {
            return new ArrayList<>();
        }
        String playlistUuid = splitAssociationDOS.get(0).getPlaylistUuid();
        String pplVersionUuid = splitAssociationDOS.get(0).getPplVersionId();
        titleUuid = splitAssociationDOS.get(0).getTitleId();
        boolean isBaseSegment = StringUtils.isEmpty(playlistUuid);
        List<String> uuids = splitAssociationDOS.stream().map(
                PlaylistSegmentSplitAssociationDO::getSegmentSplitUuid).collect(
                Collectors.toList());
        List<SegmentSplitDO> segmentSplitDOS = iSegmentSplitService.getByStatus(uuids, status);

        Map<String, Object> stringObjectMap = null;

        if (!isBaseSegment) {
            PlaylistVersionContentAssociationDO playlistVersionContentAssociationDO = contentAssociationService
                    .getAssociationsByUuid(associationUuid, pplVersionUuid);
            if (playlistVersionContentAssociationDO == null) {
                throw new BizException(ResultCodeEnum.RESOURCE_NOT_FOUND);
            }
            stringObjectMap = JSON.parseObject(playlistVersionContentAssociationDO.getExtension(),
                    new TypeReference<Map<String, Object>>() {
                    });
        }

        Map<String, Object> finalStringObjectMap = stringObjectMap;
        String finalTitleUuid = titleUuid;
        return segmentSplitDOS.stream().map(segmentSplitDO -> {
            List<ContentInfo> contentList = segmentContentViewService
                    .listContentInfo(segmentSplitDO.getUuid());

            if (!isBaseSegment) {
                if (finalStringObjectMap != null
                        && finalStringObjectMap.get("automation") != null) {
                    contentList.stream().filter(contentInfo -> ContentTypeEnum.CPL.getName()
                            .equals(contentInfo.getContentType()))
                            .forEach(contentInfo -> {
                                Map<String, Object> objectMap = JSON
                                        .parseObject(contentInfo.getExtension(),
                                                new TypeReference<Map<String, Object>>() {
                                                });
                                if (!objectMap.containsKey("automation")) {
                                    objectMap.put("automation",
                                            finalStringObjectMap.get("automation"));
                                    contentInfo.setExtension(JSON.toJSONString(objectMap));
                                }
                            });

                }
            }

            return toSegmentSplitInfo(
                    segmentSplitDO,
                    associationUuid,
                    playlistUuid,
                    pplVersionUuid,
                    finalTitleUuid,
                    contentList
            );
        }).collect(Collectors.toList());
    }

    @Override
    @Transactional
    public TitleInfo getTitleInfo(String contentAssociationUuid, String titleUuid) {
        TitleInfo titleInfo = new TitleInfo();

        List<SegmentSplitInfo> segmentSplitInfos = getByContentAssociationUuid(
                contentAssociationUuid, titleUuid);
        titleInfo.setSegmentSplitInfos(segmentSplitInfos);

        try {
            if (segmentSplitInfos.isEmpty()) {
                return titleInfo;
            }
            SegmentSplitInfo splitInfo = segmentSplitInfos.get(0);
            if (splitInfo != null) {
                String pplUuid = splitInfo.getPlaylistUuid();
                IPlaylistService iPlaylistService = SpringContextUtils
                        .getBean(IPlaylistService.class);
                PlaylistDO playlistDO = iPlaylistService.getPlaylist(pplUuid);
                titleInfo.setPplUuid(pplUuid);
                if (playlistDO != null) {
                    titleInfo.setPplName(playlistDO.getTitle());
                    titleInfo.setAutomaticallyApply(playlistDO.getAutomaticallyApply());
                }
            }

            if (StringUtils.isNotEmpty(titleUuid)) {
                com.aam.producer.playlist.sal.response.TitleInfo info = iTitleFacadeClient
                        .geTitlesByUuid(titleUuid);
                if (info != null) {
                    titleInfo.setTitleName(info.getName());
                    titleInfo.setMovieImage(info.getMovieImage());
                }
            }
            if (splitInfo != null) {
                PlaylistVersionContentAssociationDO associationDO = contentAssociationService
                        .getAssociationsByUuid(contentAssociationUuid,
                                splitInfo.getPplVersionUuid());
                if (associationDO != null) {
                    titleInfo.setSegmentName(associationDO.getTitle());
                    titleInfo.setSegmentInfo(associationDO.getExtension());
                } else {
                    SegmentDO segmentDO = iSegmentService.getById(contentAssociationUuid);
                    titleInfo.setSegmentName(segmentDO.getTitle());
                }
            }
        } catch (Exception e) {
            logger.error("getTitleInfo error", e);
            logger.error("getTitleInfo error.contentAssociationUuid:<{}> titleUuid:<{}>",
                    contentAssociationUuid, titleUuid);
        }
        return titleInfo;
    }


    @Override
    public List<SegmentSplitInfo> getSegmentSplitTree(String associationUuid, String titleUuid,
            Integer status) {
        return buildSplitRuleTree(getSegmentSplitList(associationUuid, titleUuid, status));
    }

    @Override
    public SegmentStatusEnum getStatus(String titleUuid, String associationUuid) {
        List<PlaylistSegmentSplitAssociationDO> associationDOS = segmentSplitAssociationService
                .getSegmentSplitByContentAssociationUuid(associationUuid, titleUuid);
        List<String> segmentSplitUuid = associationDOS.stream()
                .map(PlaylistSegmentSplitAssociationDO::getSegmentSplitUuid)
                .collect(Collectors.toList());
        List<SegmentSplitDO> segmentSplitDOList = iSegmentSplitService.getByIds(segmentSplitUuid);
        return getStatus(associationDOS,segmentSplitDOList);
    }

    @Override
    public SegmentStatusEnum getStatus(List<PlaylistSegmentSplitAssociationDO> associationDOS,
                                       List<SegmentSplitDO> segmentSplitDOS) {
        if (associationDOS.isEmpty()) {
            return SegmentStatusEnum.NULL;
        }
        List<String> segmentSplitUuid = associationDOS.stream()
                .map(PlaylistSegmentSplitAssociationDO::getSegmentSplitUuid)
                .collect(Collectors.toList());
        segmentSplitDOS = segmentSplitDOS.stream().filter(segmentSplitDO ->
                segmentSplitUuid.contains(segmentSplitDO.getUuid())).collect(Collectors.toList());
        Set<Integer> status = segmentSplitDOS.stream().map(SegmentSplitDO::getStatus)
                .collect(Collectors.toSet());
        if (status.contains(SegmentStatusEnum.DRAFT.getStatus()) && status
                .contains(SegmentStatusEnum.RELEASE.getStatus())) {
            return SegmentStatusEnum.DRAFT_AND_RELEASE;
        }
        if (status.contains(SegmentStatusEnum.DRAFT.getStatus())) {
            return SegmentStatusEnum.DRAFT;
        } else {
            return SegmentStatusEnum.RELEASE;
        }
    }


    @Override
    public List<String> getShowsBySegmentSplitUuid(String segmentSplitUuid) {
        PlaylistSegmentSplitAssociationDO playlistSegmentSplitAssociationDO = segmentSplitAssociationService
                .getSegmentSplitBySegmentSplitUuid(segmentSplitUuid);

        if (playlistSegmentSplitAssociationDO == null) {
            return new ArrayList<>();
        }

        List<SegmentSplitInfo> segmentSplitInfos = getByContentAssociationUuid(
                playlistSegmentSplitAssociationDO.getSegmentAssociationUuid(),
                playlistSegmentSplitAssociationDO.getTitleId());

        for (SegmentSplitInfo segmentSplitInfo : segmentSplitInfos) {
            if (segmentSplitInfo.getUuid().equals(segmentSplitUuid)) {
                return segmentSplitInfo.getShowUuids();
            }
        }
        return new ArrayList<>();
    }

    @Override
    public List<ShowAttributeGroupInfo> getShowType(String uuid) {
        PlaylistSegmentSplitAssociationDO playlistSegmentSplitAssociationDO = segmentSplitAssociationService
                .getSegmentSplitBySegmentSplitUuid(uuid);
        List<SegmentSplitInfo> segmentSplitInfos = getSegmentSplitList(
                playlistSegmentSplitAssociationDO.getSegmentAssociationUuid(),
                playlistSegmentSplitAssociationDO.getTitleId(), null);
        List<SegmentSplitInfo> treeList = buildSplitRuleTree(segmentSplitInfos);
        List<ShowAttributeGroupInfo> showAttributeGroupInfos = iPlaylistShowAttributeViewService
                .getByPplVersionUuid(playlistSegmentSplitAssociationDO.getPplVersionId());
        fillShowType(treeList, showAttributeGroupInfos);
        return findSegmentSplitInfo(segmentSplitInfos, uuid).getShowAttributeGroupInfos();
    }


    private List<SegmentSplitInfo> buildSplitRuleTree(List<SegmentSplitInfo> segmentSplitInfos) {
        List<SegmentSplitInfo> segmentSplitInfoTreeRootList = segmentSplitInfos.stream()
                .filter(segmentSplitInfo -> StringUtils.isBlank(segmentSplitInfo.getParentUuid()))
                .collect(Collectors.toList());
        segmentSplitInfoTreeRootList
                .forEach(segmentSplitInfo -> buildSplitRuleSubTree(segmentSplitInfo,
                        segmentSplitInfos));
        return segmentSplitInfoTreeRootList;
    }

    private void buildSplitRuleSubTree(SegmentSplitInfo segmentSplitInfoTreeRoot,
            List<SegmentSplitInfo> segmentSplitInfos) {
        segmentSplitInfoTreeRoot.setSiblingsAndSelf(segmentSplitInfos.stream()
                .filter(segmentSplitInfo -> Objects
                        .equals(segmentSplitInfo.getParentUuid(),
                                segmentSplitInfoTreeRoot.getParentUuid()))
                .collect(Collectors.toList()));

        segmentSplitInfos.stream()
                .filter(segmentSplitInfo -> Objects
                        .equals(segmentSplitInfo.getParentUuid(),
                                segmentSplitInfoTreeRoot.getUuid()))
                .forEach(segmentSplitInfo -> {
                    segmentSplitInfoTreeRoot.getChildren().add(segmentSplitInfo);
                    segmentSplitInfo.setParent(segmentSplitInfoTreeRoot);
                    buildSplitRuleSubTree(segmentSplitInfo, segmentSplitInfos);
                });
    }

}
