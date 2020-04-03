package com.aam.producer.playlist.biz.service.domain.impl;

import com.aam.authentication.acl.utils.UserInfoUtil;
import com.aam.producer.lib.enums.ContentTypeEnum;
import com.aam.producer.playlist.biz.enums.PlaylistStatusEnum;
import com.aam.producer.playlist.biz.service.IPlaylistSegmentSplitAssociationService;
import com.aam.producer.playlist.biz.service.IPlaylistService;
import com.aam.producer.playlist.biz.service.IPlaylistShowAttributeCombinationService;
import com.aam.producer.playlist.biz.service.IPlaylistVersionContentAssociationService;
import com.aam.producer.playlist.biz.service.IPlaylistVersionService;
import com.aam.producer.playlist.biz.service.IPosPlaylistMappingService;
import com.aam.producer.playlist.biz.service.IPplViewService;
import com.aam.producer.playlist.biz.service.ISegmentSplitContentAssociationService;
import com.aam.producer.playlist.biz.service.ITmsPlaylistService;
import com.aam.producer.playlist.biz.service.domain.IPlaylistShowAttributeViewService;
import com.aam.producer.playlist.biz.service.domain.IPlaylistVersionContentViewService;
import com.aam.producer.playlist.biz.service.domain.IPlaylistVersionViewService;
import com.aam.producer.playlist.biz.service.domain.IPlaylistViewService;
import com.aam.producer.playlist.biz.util.CommonSourcePool;
import com.aam.producer.playlist.biz.util.OrgUtil;
import com.aam.producer.playlist.protocol.request.PplFilterDTO;
import com.aam.producer.playlist.protocol.response.*;
import com.aam.producer.playlist.repository.entity.PlaylistDO;
import com.aam.producer.playlist.repository.entity.PlaylistSegmentSplitAssociationDO;
import com.aam.producer.playlist.repository.entity.PlaylistShowAttributeCombinationDO;
import com.aam.producer.playlist.repository.entity.PlaylistVersionContentAssociationDO;
import com.aam.producer.playlist.repository.entity.PlaylistVersionDO;
import com.aam.producer.playlist.repository.entity.PosPlaylistMappingDO;
import com.aam.producer.playlist.repository.entity.PplViewDO;
import com.aam.producer.playlist.repository.entity.SegmentSplitContentAssociationDO;
import com.aam.producer.playlist.repository.entity.TmsPlaylistDO;
import com.aam.producer.playlist.sal.client.IComplexFacadeClient;
import com.aam.producer.playlist.sal.client.IOrgFacadeClient;
import com.aam.producer.playlist.sal.client.ITaskFacadeClient;
import com.aam.producer.playlist.sal.client.ITitleFacadeClient;
import com.aam.producer.task.protocol.response.PPLIssueModel;
import com.aam.utils.model.PageListResult;
import com.alibaba.fastjson.JSON;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.function.Function;
import java.util.stream.Collectors;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

@Service
public class PlaylistViewServiceImpl implements IPlaylistViewService {

    private final static Logger logger = LoggerFactory.getLogger(PlaylistViewServiceImpl.class);
    private static Map<Integer, Integer> statusMap = new HashMap<>();

    static {
        statusMap.put(1, 2);
        statusMap.put(2, 1);
        statusMap.put(3, 3);
    }

    private static Map<Integer, Integer> typeMap = new HashMap<>();

    static {
        typeMap.put(1, 1);
        typeMap.put(2, 0);
    }

    private final IPlaylistService iPlaylistService;
    private final IPplViewService iPplViewService;
    private final ITmsPlaylistService tmsPlaylistService;
    private final IPlaylistVersionViewService iPlaylistVersionViewService;
    private final IPlaylistVersionService iPlaylistVersionService;
    private final IPlaylistShowAttributeCombinationService combinationService;
    private final IPlaylistShowAttributeViewService viewService;
    private final IPosPlaylistMappingService iPosPlaylistMappingService;
    private final ITaskFacadeClient iTaskFacadeClient;
    private final IComplexFacadeClient iComplexFacadeClient;
    private final IOrgFacadeClient iOrgFacadeClient;
    private final ITitleFacadeClient iTitleFacadeClient;
    private final ISegmentSplitContentAssociationService splitContentAssociationService;
    private final IPlaylistVersionContentAssociationService contentAssociationService;
    private final IPlaylistSegmentSplitAssociationService splitAssociationService;
    private final IPlaylistVersionContentViewService iPlaylistVersionContentViewService;
    @Value(value = "${thunderstorm.pressureMeasurement}")
    private Boolean pressureMeasurement;

    @Autowired
    public PlaylistViewServiceImpl(IPlaylistService iPlaylistService,
            IPlaylistVersionViewService iPlaylistVersionViewService,
            IPlaylistShowAttributeCombinationService combinationService,
            IPlaylistVersionService iPlaylistVersionService,
            IPplViewService iPplViewService,
            ITmsPlaylistService tmsPlaylistService,
            IPosPlaylistMappingService iPosPlaylistMappingService,
            IPlaylistShowAttributeViewService viewService,
            ITaskFacadeClient iTaskFacadeClient,
            IComplexFacadeClient iComplexFacadeClient,
            IOrgFacadeClient iOrgFacadeClient,
            ITitleFacadeClient iTitleFacadeClient,
            ISegmentSplitContentAssociationService splitContentAssociationService,
            IPlaylistVersionContentAssociationService contentAssociationService,
            IPlaylistSegmentSplitAssociationService splitAssociationService,
            IPlaylistVersionContentViewService iPlaylistVersionContentViewService) {
        this.iPlaylistService = iPlaylistService;
        this.iPlaylistVersionViewService = iPlaylistVersionViewService;
        this.combinationService = combinationService;
        this.iPlaylistVersionService = iPlaylistVersionService;
        this.iPplViewService = iPplViewService;
        this.tmsPlaylistService = tmsPlaylistService;
        this.iPosPlaylistMappingService = iPosPlaylistMappingService;
        this.viewService = viewService;
        this.iTaskFacadeClient = iTaskFacadeClient;
        this.iComplexFacadeClient = iComplexFacadeClient;
        this.iOrgFacadeClient = iOrgFacadeClient;
        this.iTitleFacadeClient = iTitleFacadeClient;
        this.splitContentAssociationService = splitContentAssociationService;
        this.contentAssociationService = contentAssociationService;
        this.splitAssociationService = splitAssociationService;
        this.iPlaylistVersionContentViewService = iPlaylistVersionContentViewService;
    }

    @Override
    public PlaylistInfo getPlaylist(String playlistUuid) {
        PlaylistDO playlistDO = iPlaylistService.getById(playlistUuid);
        return toPlaylistInfo(playlistDO);
    }

    @Override
    public PlaylistInfo getPlaylist(String playlistUuid, String versions, String titleId) {
        PlaylistInfo playlistInfo = getPlaylist(playlistUuid);
        if (playlistInfo == null) {
            return null;
        }
        if (StringUtils.isNotBlank(versions)) {
            List<PlaylistVersionInfo> versionInfos = iPlaylistVersionViewService
                    .getPlaylistVersions(playlistUuid, versions, titleId);
            playlistInfo.setVersions(versionInfos);
        }
        Map<ContentInfo, Future<List<PPLIssueModel>>> result = new HashMap<>();
        playlistInfo.getVersions().stream().filter(playlistVersionInfo -> Objects
                .equals(playlistVersionInfo.getStatus(), PlaylistStatusEnum.RELEASE.getStatusStr()))
                .forEach(playlistVersionInfo -> playlistVersionInfo.getContentList().stream()
                        .filter(contentInfo -> ContentTypeEnum.SEGMENT.getName()
                                .equals(contentInfo.getContentType())).forEach(contentInfo -> {

                            String userId = UserInfoUtil.get().getUserId();
                            String orgId = OrgUtil.orgContenter.get();

                            Callable<List<PPLIssueModel>> work = () -> iTaskFacadeClient
                                    .queryOnePplIssue(userId, titleId, playlistUuid, orgId,
                                            contentInfo.getContentAssociationUuid());
                            Future<List<PPLIssueModel>> future = CommonSourcePool
                                    .getExecutorService().submit(work);
                            result.put(contentInfo, future);
                        }));

        result.forEach((contentInfo, future) -> {
            try {
                List<PPLIssueModel> pplIssueModels = future.get(3, TimeUnit.SECONDS);
                if (CollectionUtils.isNotEmpty(pplIssueModels)) {
                    PPLIssueModel pplIssueModel = pplIssueModels.stream().min(
                            Comparator.comparingInt(PPLIssueModel::getIssueLevel)).get();
                    contentInfo.setIssueLevel(pplIssueModel.getIssueLevel());
                    contentInfo.setIssueType(pplIssueModel.getIssueType());
                }
            } catch (InterruptedException | ExecutionException | TimeoutException e) {
                logger.error("get issue from task error.", e);
            }
        });
        return playlistInfo;
    }

    @Override
    public PlaylistDO getPlaylist(Long code) {
        List<PlaylistShowAttributeCombinationDO> combinationDOS = combinationService
                .getCombinationsBySumCode(code);
        if (combinationDOS.isEmpty()) {
            return null;
        }
        PlaylistVersionDO releasePlaylistVersionDO = combinationDOS.stream()
                .map(playlistShowAttributeCombinationDO -> iPlaylistVersionService
                        .getById(playlistShowAttributeCombinationDO.getPlaylistUuid()))
                .filter(playlistVersionDO -> PlaylistStatusEnum.RELEASE.getStatus()
                        .equals(playlistVersionDO.getStatus())).findFirst().orElse(null);
        if (releasePlaylistVersionDO == null) {
            return null;
        }
        return iPlaylistService.getPlaylist(releasePlaylistVersionDO.getPlaylistUuid());
    }

    @Override
    public PageListResult<PplViewInfo> search(Integer pageNum, Integer pageSize, String search,
            String useId) {
        PplFilterDTO filterPplModel = JSON.parseObject(search, PplFilterDTO.class);

        if (filterPplModel == null) {
            filterPplModel = new PplFilterDTO();
        }
        QueryWrapper<PplViewDO> wrapper = new QueryWrapper<>();
        if (StringUtils.isNotBlank(search)) {
            if (StringUtils.isNotBlank(filterPplModel.getTitle())) {
                wrapper.like("title", filterPplModel.getTitle());
            }
            if (filterPplModel.getStatus() != null && filterPplModel.getStatus().size() > 0) {
                List<Integer> statusList = filterPplModel.getStatus().stream()
                        .map(integer -> statusMap.get(integer))
                        .collect(Collectors.toList());
                wrapper.in("status", statusList);
            }
            if (filterPplModel.getTypes() != null && filterPplModel.getTypes().size() > 0) {
                wrapper.in("automatically_apply",
                        filterPplModel.getTypes().stream().map(integer -> typeMap.get(integer))
                                .collect(Collectors.toList()));
            }
            if (filterPplModel.getShows() != null && filterPplModel.getShows().size() > 0) {
                if (filterPplModel.getShows().size() != 2) {
                    if (filterPplModel.getShows().get(0) == 1) {
                        wrapper.eq("shows", 0);
                    } else {
                        wrapper.gt("shows", 0);
                    }
                }
            }
            wrapper.orderByDesc("automatically_apply");
            if (StringUtils.isNotBlank(filterPplModel.getOrderByName())) {
                if (filterPplModel.isOrderByDesc()) {
                    switch (filterPplModel.getOrderByName().trim()) {
                        case "name":
                            wrapper.orderByDesc("title");
                            break;
                        case "lastModified":
                            wrapper.orderByDesc("last_modified");
                            break;
                        case "showCount":
                            wrapper.orderByDesc("shows");
                            break;
                        default:
                    }
                } else {
                    switch (filterPplModel.getOrderByName().trim()) {
                        case "name":
                            wrapper.orderByAsc("title");
                            break;
                        case "lastModified":
                            wrapper.orderByAsc("last_modified");
                            break;
                        case "showCount":
                            wrapper.orderByAsc("shows");
                        default:
                    }
                }
            }
        }
        Page<PplViewDO> page = new Page<>(pageNum, pageSize);
        IPage<PplViewDO> iPage = iPplViewService.page(page, wrapper);
        List<PplViewDO> pplViewDOS = iPage.getRecords();
        List<String>  pplVersionUuids = pplViewDOS.stream().map(PplViewDO::getReleaseVersionUuid)
                .collect(Collectors.toList());
        List<ShowAttributeGroupInfo> showTypeList = viewService.getByPplVersionUuids(pplVersionUuids);
        List<PplViewInfo> pplViewInfos = pplViewDOS.stream().map(pplViewDO -> {
            List<ShowAttributeGroupInfo> showTypes = showTypeList.stream().filter(showAttributeGroupInfo ->
                    Objects.equals(pplViewDO.getReleaseVersionUuid(),showAttributeGroupInfo.getPplVersionUuid()))
                    .collect(Collectors.toList());
            return toPplViewInfo(pplViewDO, showTypes);
        }).collect(Collectors.toList());
        countIssue(pplViewInfos, useId, null);
        computeStaticPplShowType(pplViewInfos);
        com.aam.utils.model.page.Page<PplViewInfo> tPage = buildPage(iPage.getTotal(),
                iPage.getCurrent(), iPage.getSize(), pplViewInfos);
        return new PageListResult<>(tPage);
    }

    private com.aam.utils.model.page.Page<PplViewInfo> buildPage(long total, long current,
            long size, List<PplViewInfo> pplViewInfos) {
        com.aam.utils.model.page.Page<PplViewInfo> tPage = new com.aam.utils.model.page.Page<>();
        tPage.setItems(total);
        tPage.setPageNum(current);
        tPage.setPageSize((int) size);
        tPage.setRecords(pplViewInfos);
        return tPage;
    }

    private void computeStaticPplShowType(List<PplViewInfo> pplViewInfos) {
        List<PplViewInfo> staticPpls = pplViewInfos.stream()
                .filter(pplViewInfo -> pplViewInfo.getAutomaticallyApply() == 0)
                .filter(pplViewInfo -> !PlaylistStatusEnum.DRAFT.getStatusStr()
                        .equals(pplViewInfo.getStatus())).collect(Collectors.toList());
        List<String> staticPplVersionIds = staticPpls.stream()
                .map(PplViewInfo::getReleaseVersionUuid).collect(Collectors.toList());

        if (!staticPplVersionIds.isEmpty()) {
            List<ContentInfo> contentInfos = iPlaylistVersionContentViewService
                    .getByPplVersionUuids(staticPplVersionIds);
            staticPpls.forEach(pplViewInfo -> {
                boolean b = contentInfos.stream().filter(contentInfo -> Objects
                        .equals(contentInfo.getVersion(), pplViewInfo.getReleaseVersionUuid()))
                        .filter(contentInfo -> {
                            String content = contentInfo.getExtension();
                            return StringUtils.isNotEmpty(content);
                        })
                        .map(contentInfo -> {
                            String content = contentInfo.getExtension();
                            return JSON.parseObject(content);
                        })
                        .filter(jsonObject ->
                                Objects.equals(jsonObject.getString("type"), "composition"))
                        .anyMatch(jsonObject -> jsonObject.getBoolean("stereoscopic"));
                pplViewInfo.setIs3d(b);
            });

        }
    }

    private void countIssue(List<PplViewInfo> pplViewInfos, String useId, String titleUuid) {
        if (Boolean.TRUE.equals(pressureMeasurement)){return;}
        List<String> pplIds = pplViewInfos.stream().map(PplViewInfo::getPplUuid)
                .collect(Collectors.toList());
        if (!pplIds.isEmpty()) {
            String orgId= UserInfoUtil.getOrganizationId();
            Future<Map<String, PPLIssueModel>> future = CommonSourcePool
                    .getExecutorService().submit(()-> iTaskFacadeClient.queryPplIssue(useId, titleUuid, pplIds, orgId));
            Map<String, PPLIssueModel> pplIssueModelMap;
            try {
                pplIssueModelMap = future.get(2, TimeUnit.SECONDS);
            } catch (Exception e) {
                logger.info("get issue exception.",e);
                pplIssueModelMap = new HashMap<>();
            }
            Map<String, PPLIssueModel> finalPplIssueModelMap = pplIssueModelMap;
            pplViewInfos.forEach(pplViewInfo -> {
                PPLIssueModel pplIssueModel = finalPplIssueModelMap.get(pplViewInfo.getPplUuid());
                if (pplIssueModel != null) {
                    pplViewInfo.setIssueLevel(pplIssueModel.getIssueLevel());
                    pplViewInfo.setIssueType(pplIssueModel.getIssueType());
                }
            });
        }
    }

    @Override
    public PageListResult<PplViewInfo> searchTpl(Integer pageNum, Integer pageSize, String search,
            String organizationId) {
        PplFilterDTO filterPplModel = JSON.parseObject(search, PplFilterDTO.class);

        if (filterPplModel == null) {
            filterPplModel = new PplFilterDTO();
        }

        QueryWrapper<TmsPlaylistDO> wrapper = new QueryWrapper<>();
        if (StringUtils.isNotEmpty(filterPplModel.getTitle())) {
            wrapper.like("title", filterPplModel.getTitle());
        }
        if (!UserInfoUtil.isAdmin()) {
            if (CollectionUtils.isEmpty(UserInfoUtil.getGroups())) {
                return new PageListResult<>(new com.aam.utils.model.page.Page<>());
            }
            List<String> complexUuids = iOrgFacadeClient.getComplexUuid(UserInfoUtil.getGroups());
            if (CollectionUtils.isEmpty(complexUuids)) {
                return new PageListResult<>(new com.aam.utils.model.page.Page<>());
            }
            wrapper.in("source_complex_id", complexUuids);
        }
        wrapper.eq("source", 2);
        Page<TmsPlaylistDO> page = new Page<>(pageNum, pageSize);
        wrapper.select(TmsPlaylistDO.class,x->!x.getColumn().equals("json")&&!x.getColumn().equals("validation"));
        IPage<TmsPlaylistDO> iPage = tmsPlaylistService.page(page, wrapper);

        List<PplViewInfo> pplViewInfos = iPage.getRecords().stream().map(this::toPplViewInfo)
                .collect(Collectors.toList());

        com.aam.utils.model.page.Page<PplViewInfo> tPage = buildPage(iPage.getTotal(),
                iPage.getCurrent(), iPage.getSize(), pplViewInfos);

        setComplexInfo(pplViewInfos);

        countPos(pplViewInfos);

        return new PageListResult<>(tPage);
    }

    private void countPos(List<PplViewInfo> pplViewInfos) {
        Set<String> uuids = pplViewInfos.stream().map(PplViewInfo::getPlaylistUuid)
                .collect(Collectors.toSet());

        if (!uuids.isEmpty()) {
            try {
                QueryWrapper<PosPlaylistMappingDO> wrapper1 = new QueryWrapper<>();
                wrapper1.in("tpl_uuid", uuids);
                wrapper1.gt("pos_start", System.currentTimeMillis());
                List<PosPlaylistMappingDO> posPlaylistMappingDOS = iPosPlaylistMappingService
                        .list(wrapper1);
                Map<String, Long> showCountMap = new HashMap<>();
                uuids.forEach(s -> {
                    Long size = posPlaylistMappingDOS.stream().filter(posPlaylistMappingDO -> s
                            .equals(posPlaylistMappingDO.getTplUuid())).count();
                    showCountMap.put(s, size);
                });
                pplViewInfos.forEach(pplViewInfo -> pplViewInfo
                        .setShows(showCountMap.get(pplViewInfo.getPlaylistUuid())));
            } catch (Exception e) {
                logger.error("Setting show size fail.", e);
            }
        }
    }

    private void setComplexInfo(List<PplViewInfo> pplViewInfos) {
        Set<String> complexUuids = pplViewInfos.stream()
                .map(PplViewInfo::getComplexUuid).collect(Collectors.toSet());
        if (!complexUuids.isEmpty()) {
            try {
                Map<String, String> stringStringMap = iComplexFacadeClient
                        .getComplexNameMap(new ArrayList<>(complexUuids));
                pplViewInfos.forEach(pplViewInfo -> pplViewInfo
                        .setComplexName(stringStringMap.get(pplViewInfo.getComplexUuid())));
            } catch (Exception e) {
                logger.error("Setting complex name fail.", e);
            }
        }
    }

    @Override
    public PageListResult<PplViewInfo> searchByTitle(String titleUuid, String title, String useId) {
        //获取title下的场次
        List<PosInfo> posInfos = iPosPlaylistMappingService
                .getPosByTitle(titleUuid);
        //获取场次对应的播放列表
        Set<String> pplUuids = posInfos.stream()
                .map(PosInfo::getPplUuid).collect(
                        Collectors.toSet());

        return getPageListResult(pplUuids, title, (playlistDOS) -> {
            //转换数据
            List<PplViewInfo> pplViewInfos = playlistDOS.stream()
                    .map(playlistDO -> toPplViewInfo(playlistDO,
                            countPosByPlaylistUuid(posInfos, playlistDO.getUuid())))
                    .collect(Collectors.toList());
            //统计问题
            countIssue(pplViewInfos, useId, titleUuid);
            return pplViewInfos;
        });
    }

    @Override
    public PageListResult<PplViewInfo> searchByContentId(String title, String contentId) {
        Set<String> pplUuids = new HashSet<>();
        List<PlaylistVersionContentAssociationDO> contentAssociationDOS = contentAssociationService
                .getAssociationsByContentUuid(contentId);
        contentAssociationDOS.forEach(
                contentAssociationDO -> pplUuids.add(contentAssociationDO.getPlaylistUuid()));

        Set<String> segmentSplitUUIDs = new HashSet<>();
        List<SegmentSplitContentAssociationDO> associationDOS = splitContentAssociationService
                .getSplitAssociationsByContentUuid(contentId);
        associationDOS.forEach(splitContentAssociationDO -> segmentSplitUUIDs
                .add(splitContentAssociationDO.getSegmentSplitUuid()));

        if (CollectionUtils.isNotEmpty(segmentSplitUUIDs)) {
            List<PlaylistSegmentSplitAssociationDO> playlistSegmentSplitAssociationDOS = splitAssociationService
                    .getSegmentSplitBySegmentSplitUuids(segmentSplitUUIDs);
            playlistSegmentSplitAssociationDOS.forEach(x -> pplUuids.add(x.getPlaylistUuid()));
        }

        return getPageListResult(pplUuids, title, (playlistDOS) -> {
            List<String> playlistUuids = playlistDOS.stream().map(PlaylistDO::getUuid).collect(Collectors.toList());
            Map<String, Long> posCount = iPosPlaylistMappingService.getPosCount(playlistUuids);
            return playlistDOS.stream()
                    .map(playlistDO -> toPplViewInfo(playlistDO,
                            posCount.get(playlistDO.getUuid()))).collect(Collectors.toList());
        });
    }

    @Override
    public List<TitleInfo1> getPlaylistSegmentTitleInfo(String playlistUuid,
            String associationUuid) {
        PlaylistDO playlistDO = iPlaylistService.getById(playlistUuid);
        PlaylistVersionContentAssociationDO associationDO = iPlaylistVersionContentViewService
                .getByContentAssociationUuid(associationUuid);
        List<PosPlaylistMappingDO> posPlaylistMappingDOS = iPosPlaylistMappingService
                .getMappingsByPpl(false, playlistUuid);
        Set<String> titleUuids =
                posPlaylistMappingDOS.stream().map(PosPlaylistMappingDO::getTitleUuid).filter(s -> !Objects.isNull(s))
                        .collect(Collectors.toSet());
        if (!titleUuids.isEmpty()) {
            List<com.aam.producer.playlist.sal.response.TitleInfo> titleInfos ;
            try{
                titleInfos = iTitleFacadeClient
                    .geTitlesByUuids(titleUuids, iOrgFacadeClient.getComplexGroupUuid(OrgUtil.orgContenter.get()));
            }catch (Exception e){
                logger.error("get title info exception. playlistUuid:<{}> associationUuid:<{}>",
                        playlistUuid, associationUuid, e);
                return new ArrayList<>();
            }
            return titleInfos.stream().map(titleInfo -> {
                        TitleInfo1 titleInfo1 = new TitleInfo1();
                        titleInfo1.setMovieImage(titleInfo.getMovieImage());
                        titleInfo1.setPplName(playlistDO.getTitle());
                        titleInfo1.setTitleName(titleInfo.getName());
                        titleInfo1.setSegmentUuid(associationUuid);
                        titleInfo1.setSegmentName(associationDO.getTitle());
                        titleInfo1.setTitleUuid(titleInfo.getUuid());
                        List<PPLIssueModel> pplIssueModels = iTaskFacadeClient
                                .queryOnePplIssue(UserInfoUtil.get().getUserId(),
                                        titleInfo.getUuid(), playlistUuid,
                                        OrgUtil.orgContenter.get(), associationUuid);
                        if (CollectionUtils.isNotEmpty(pplIssueModels)) {
                            PPLIssueModel pplIssueModel = pplIssueModels.stream().min(
                                    Comparator.comparingInt(PPLIssueModel::getIssueLevel)).get();
                            titleInfo1.setIssueLevel(pplIssueModel.getIssueLevel());
                            titleInfo1.setIssueType(pplIssueModel.getIssueType());
                        }
                        return titleInfo1;
                    }).collect(Collectors.toList());

        }
        return new ArrayList<>();
    }

    private PageListResult<PplViewInfo> getPageListResult(Set<String> pplUuids, String title,
            Function<List<PlaylistDO>, List<PplViewInfo>> function) {
        com.aam.utils.model.page.Page<PplViewInfo> tPage = new com.aam.utils.model.page.Page<>();
        if (pplUuids.isEmpty()) {
            return new PageListResult<>(tPage);
        }
        //查询播放列表信息信息
        QueryWrapper<PlaylistDO> wrapper = new QueryWrapper<>();
        wrapper.in("uuid", pplUuids);
        if (StringUtils.isNotBlank(title)) {
            wrapper.like("title", title);
        }
        List<PlaylistDO> playlistDOS = iPlaylistService.list(wrapper);
        List<PplViewInfo> pplViewInfos = function.apply(playlistDOS.stream()
                .filter(playlistDO -> !Objects
                        .equals(PlaylistStatusEnum.DRAFT.getStatus(), playlistDO.getStatus()))
                .collect(Collectors.toList()));
        tPage.setRecords(pplViewInfos);
        return new PageListResult<>(tPage);
    }

    private long countPosByPlaylistUuid(List<PosInfo> posInfos,
            String playlistUuid) {
        return posInfos.stream().filter(posInfo -> playlistUuid
                .equals(posInfo.getPplUuid()))
                .collect(Collectors.toSet()).size();
    }

}
