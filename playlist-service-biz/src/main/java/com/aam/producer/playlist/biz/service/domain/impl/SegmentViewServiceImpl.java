package com.aam.producer.playlist.biz.service.domain.impl;

import com.aam.producer.lib.enums.ContentTypeEnum;
import com.aam.producer.lib.enums.SegmentTypeEnum;
import com.aam.producer.playlist.biz.enums.PlaylistStatusEnum;
import com.aam.producer.playlist.biz.enums.ResultCodeEnum;
import com.aam.producer.playlist.biz.enums.SegmentStatusEnum;
import com.aam.producer.playlist.biz.service.IPlaylistVersionContentAssociationService;
import com.aam.producer.playlist.biz.service.IPosPlaylistMappingService;
import com.aam.producer.playlist.biz.service.ISegmentService;
import com.aam.producer.playlist.biz.service.domain.IPlaylistViewService;
import com.aam.producer.playlist.biz.service.domain.ISegmentViewService;
import com.aam.producer.playlist.biz.util.OrgUtil;
import com.aam.producer.playlist.protocol.request.SegmentSearchDTO;
import com.aam.producer.playlist.protocol.response.PlaylistInfo;
import com.aam.producer.playlist.protocol.response.SegmentInfo;
import com.aam.producer.playlist.repository.entity.PlaylistVersionContentAssociationDO;
import com.aam.producer.playlist.repository.entity.PosPlaylistMappingDO;
import com.aam.producer.playlist.repository.entity.SegmentDO;
import com.aam.producer.playlist.sal.client.IOrgFacadeClient;
import com.aam.producer.playlist.sal.client.ITitleFacadeClient;
import com.aam.producer.playlist.sal.response.TitleInfo;
import com.aam.utils.exception.BizException;
import com.alibaba.fastjson.JSON;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class SegmentViewServiceImpl implements ISegmentViewService {

    private final ISegmentService iSegmentService;
    private final IPosPlaylistMappingService iPosMappingService;
    private final IPlaylistVersionContentAssociationService versionContentAssociationService;
    private final ITitleFacadeClient iTitleFacadeClient;
    private final IOrgFacadeClient iOrgFacadeClient;
    private final IPlaylistViewService iPlaylistViewService;

    @Autowired
    public SegmentViewServiceImpl(ISegmentService iSegmentService,
            IPosPlaylistMappingService iPosMappingService,
            IPlaylistVersionContentAssociationService versionContentAssociationService,
            ITitleFacadeClient iTitleFacadeClient,
            IPlaylistViewService iPlaylistViewService,
            IOrgFacadeClient iOrgFacadeClient) {
        this.iSegmentService = iSegmentService;
        this.iPosMappingService = iPosMappingService;
        this.versionContentAssociationService = versionContentAssociationService;
        this.iTitleFacadeClient = iTitleFacadeClient;
        this.iOrgFacadeClient = iOrgFacadeClient;
        this.iPlaylistViewService = iPlaylistViewService;
    }

    @Override
    public SegmentInfo getAutomaticSegment() {
        SegmentDO segmentDO = iSegmentService.getAutomaticSegment();
        if (segmentDO == null) {
            throw new BizException(ResultCodeEnum.NOT_FIND_AUTOMATIC_SEGMENT);
        }
        SegmentInfo segmentInfo = toSegmentInfo(segmentDO);
        // add visual audio auto change on-off,default on
        segmentInfo.setVisualAutomation(true);
        segmentInfo.setAudioAutomation(true);
        return segmentInfo;
    }

    @Override
    public SegmentInfo getSegment(String uuid) {
        return toSegmentInfo(iSegmentService.getSegmentDO(uuid));
    }

    @Override
    public List<SegmentInfo> getAllSegment(String search) {
        //判断是否存在rating_segment,不存在添加
        List<SegmentDO> segmentDOS;
        if (StringUtils.isNotEmpty(search)) {
            SegmentSearchDTO searchDTO = JSON.parseObject(search, SegmentSearchDTO.class);
            segmentDOS = iSegmentService
                    .searchSegment(searchDTO.getTypes(), searchDTO.getTitle());
        } else {
            segmentDOS = iSegmentService
                    .searchSegment(null, null);
        }
        return segmentDOS.stream().map(this::toSegmentInfo).collect(Collectors.toList());
    }

    @Override
    @Deprecated
    public List<String> getApiSegmentNames() {
        return iSegmentService.getApiSegmentNames();
    }

    @Override
    public List<SegmentInfo> getAllSplitWeekSegment() {
        return iSegmentService.getAllSplitWeekSegment().stream().map(this::toSegmentInfo).collect(
                Collectors.toList());
    }

    @Override
    public List<TitleInfo> getAllSegmentDetail(String search) {
        //1.获取所有非API segment
        List<SegmentDO> segmentDOS = iSegmentService.searchSegment(null, null).stream()
                .filter(segmentDO -> !SegmentTypeEnum.API_SEGMENT.getCode()
                        .equals(segmentDO.getType())).collect(
                        Collectors.toList());
        if (CollectionUtils.isEmpty(segmentDOS)) {
            return null;
        }
        //2.根据segment获取ppl
        List<PlaylistVersionContentAssociationDO> contentAssociationDOS = versionContentAssociationService
                .getAssociationsByContentUuids(segmentDOS.stream().map(
                        SegmentDO::getUuid).collect(
                        Collectors.toList()));
        List<String> pplUuids = contentAssociationDOS.stream()
                .map(PlaylistVersionContentAssociationDO::getPlaylistUuid).distinct()
                .collect(Collectors.toList());
        //3.根据ppl获取title
        List<PosPlaylistMappingDO> posPlaylistMappingDOS = pplUuids.stream()
                .flatMap(pplUuid -> iPosMappingService.getMappingsByPpl(false, pplUuid).stream())
                .collect(Collectors.toList());
        Map<String, List<PosPlaylistMappingDO>> map = posPlaylistMappingDOS.stream()
                .collect(Collectors.groupingBy(
                        PosPlaylistMappingDO::getTitleUuid));
        Map<String, List<String>> titlePplsMap = new HashMap<>();
        map.forEach((s, posPlaylistMappingDOS1) -> {
            titlePplsMap.put(s, posPlaylistMappingDOS1.stream()
                    .map(PosPlaylistMappingDO::getPplUuid).distinct()
                    .collect(
                            Collectors.toList()));
        });
        Set<String> titleUuids = posPlaylistMappingDOS.stream()
                .map(PosPlaylistMappingDO::getTitleUuid).collect(Collectors.toSet());

        List<TitleInfo> titleInfos = iTitleFacadeClient
                .geTitlesByUuids(titleUuids, iOrgFacadeClient.getComplexGroupUuid(
                        OrgUtil.orgContenter.get()));
        //4.组装数据
        titleInfos.forEach(titleInfo -> {
            List<String> pplUUids = titlePplsMap.get(titleInfo.getUuid());
            titleInfo.setPlaylistInfos(
                    pplUUids.stream().map(s -> {
                        PlaylistInfo playlistInfo = iPlaylistViewService
                                .getPlaylist(s, PlaylistStatusEnum.RELEASE.getStatusStr(),
                                        titleInfo.getUuid());
                        playlistInfo.getVersions().forEach(playlistVersionInfo -> {
                            playlistVersionInfo.setContentList(
                                    playlistVersionInfo.getContentList().stream()
                                            .filter(contentInfo -> ContentTypeEnum.SEGMENT.getName()
                                                    .equals(contentInfo.getContentType()) &&
                                                    !SegmentTypeEnum.API_SEGMENT.getName()
                                                            .equals(contentInfo
                                                                    .getContentKind()))
                                            .filter(contentInfo ->
                                                    !SegmentStatusEnum.RELEASE.getStatusStr()
                                                            .equals(contentInfo.getStatus()))
                                            .collect(Collectors.toList()));
                        });
                        return playlistInfo;
                    }).filter(playlistInfo -> playlistInfo.getVersions().stream().anyMatch(
                            playlistVersionInfo -> CollectionUtils
                                    .isNotEmpty(playlistVersionInfo.getContentList())))
                            .collect(Collectors.toList())
            );
        });
        return titleInfos.stream().filter(titleInfo -> titleInfo.getPlaylistInfos().stream()
                .anyMatch(playlistInfo -> {
                    return playlistInfo.getVersions().stream().anyMatch(
                            playlistVersionInfo -> CollectionUtils
                                    .isNotEmpty(playlistVersionInfo.getContentList()));
                })).collect(Collectors.toList());
    }

}
