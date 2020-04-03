package com.aam.producer.playlist.biz.service.domain.impl;

import com.aam.producer.playlist.biz.service.IPlaylistService;
import com.aam.producer.playlist.biz.service.domain.IMessageQueueService;
import com.aam.producer.playlist.biz.service.domain.ITaskReactorService;
import com.aam.producer.playlist.biz.util.OrgUtil;
import com.aam.producer.playlist.protocol.message.SegmentWarningDTO;
import com.aam.producer.playlist.repository.entity.PlaylistDO;
import com.aam.producer.task.protocol.enums.TaskReportType;
import com.aam.producer.task.protocol.enums.TaskTypeEnum;
import com.aam.producer.task.protocol.request.SyncReportModel;
import com.alibaba.fastjson.JSON;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class TaskReactorServiceImpl implements ITaskReactorService {

    private final IMessageQueueService mqService;

    private final IPlaylistService iPlaylistService;

    @Autowired
    public TaskReactorServiceImpl(IMessageQueueService mqService,IPlaylistService iPlaylistService) {
        this.mqService = mqService;
        this.iPlaylistService = iPlaylistService;
    }

    @Override
    public void sendSegmentSplitEvent(String pplUuid, String titleUuid,
            String contentAssociationUuid, TaskTypeEnum type, Boolean deleted,
            String segmentType) {
        SyncReportModel syncReportModel = new SyncReportModel();
        syncReportModel.setOrganizationId(OrgUtil.orgContenter.get());
        syncReportModel.setReportedAt(System.currentTimeMillis());
        syncReportModel.setTaskType(TaskReportType.PPL_WARNING.getReportType());
        SegmentWarningDTO segmentWarningDTO = new SegmentWarningDTO();
        segmentWarningDTO.setContentAssociationUuid(contentAssociationUuid);
        if (type != null) {
            segmentWarningDTO.setTaskType(type.getTaskType());
        }
        segmentWarningDTO.setTitleUuid(titleUuid);
        if (StringUtils.isNotEmpty(pplUuid)){
            PlaylistDO playlistDO = iPlaylistService.getPlaylist(pplUuid);
            segmentWarningDTO.setPplTitle(playlistDO.getTitle());
        }
        segmentWarningDTO.setDeleted(deleted);
        segmentWarningDTO.setPplUuid(pplUuid);
        segmentWarningDTO.setSegmentType(segmentType);
        syncReportModel.setPayload(JSON.toJSONString(segmentWarningDTO));
        mqService.segmentWarningData(syncReportModel);
    }

}
