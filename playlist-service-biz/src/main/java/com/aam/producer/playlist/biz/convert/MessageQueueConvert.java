package com.aam.producer.playlist.biz.convert;

import com.aam.producer.lib.enums.ContentTypeEnum;
import com.aam.producer.lib.enums.SegmentTypeEnum;
import com.aam.producer.playlist.biz.model.AutomationModel;
import com.aam.producer.playlist.common.PlaylistConst;
import com.aam.producer.playlist.protocol.message.PosMappingBatchRequestDTO;
import com.aam.producer.playlist.protocol.message.PosMappingRequestDTO;
import com.aam.producer.playlist.protocol.message.PplContentsDataDTO;
import com.aam.producer.playlist.protocol.message.TPlaylistDTO;
import com.aam.producer.playlist.protocol.message.TPlaylistDeleteRequestDTO;
import com.aam.producer.playlist.protocol.message.TPlaylistDeletionDTO;
import com.aam.producer.playlist.protocol.message.TPlaylistHashSyncRequestDTO;
import com.aam.producer.playlist.protocol.message.TPlaylistSendRequestDTO;
import com.aam.producer.playlist.protocol.message.TPlaylistSyncRequestDTO;
import com.aam.producer.playlist.protocol.message.TPlaylistWarningDTO;
import com.aam.producer.playlist.protocol.message.TPlaylistWarningDTO.IssueDetail;
import com.aam.producer.playlist.protocol.message.TPlaylistWarningDTO.TPlaylistWarningDetail;
import com.aam.producer.playlist.protocol.request.PosFetchDTO;
import com.aam.producer.playlist.repository.entity.PlaylistSendLogDO;
import com.aam.producer.playlist.repository.entity.TmsPlaylistDO;
import com.aam.producer.task.protocol.enums.TaskReportType;
import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONObject;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Mappings;
import org.mapstruct.factory.Mappers;

/**
 * playlist message convert
 *
 * @author oliver.lo
 * @since 2019/7/9 1:08 PM
 */
@Mapper
public interface MessageQueueConvert {

    MessageQueueConvert MAPPER = Mappers.getMapper(MessageQueueConvert.class);

    default TPlaylistWarningDTO toPlaylistWarningDTO(TmsPlaylistDO tmsPlaylistDO,
            List<IssueDetail> issueDetails, String pplTitle) {
        TPlaylistWarningDTO dto = new TPlaylistWarningDTO();
        dto.setOrganizationId(tmsPlaylistDO.getOrganizationId());
        dto.setTaskType(TaskReportType.PLAYLIST_WARNING.getReportType());

        TPlaylistWarningDetail payload = new TPlaylistWarningDetail();
        String json = tmsPlaylistDO.getJson();
        if (StringUtils.isNotEmpty(json)) {
            payload = JSON.parseObject(json, TPlaylistWarningDetail.class);
        }
        payload.setPplUuid(tmsPlaylistDO.getSourcePplId());
        payload.setPplTitle(pplTitle);
        payload.setUuid(tmsPlaylistDO.getPlaylistUuid());
        payload.setContentIds(JSON.parseArray(tmsPlaylistDO.getContentIds(), String.class));
        payload.setIssueDetails(issueDetails);
        dto.setPayload(payload);

        dto.setReported(System.currentTimeMillis());
        return dto;
    }

    default TPlaylistSendRequestDTO toPlaylistSendRequest(PlaylistSendLogDO logDO) {
        TPlaylistSendRequestDTO request = new TPlaylistSendRequestDTO();
        request.setComplexUuid(logDO.getComplexId());
        request.setDeviceUuid(logDO.getDeviceUuid());
        request.setReceiptUuid(logDO.getReceiptUuid());
        request.setPlaylist(toPlaylistDtoForSite(logDO));
        return request;
    }

    default TPlaylistDTO toPlaylistDtoForSite(PlaylistSendLogDO logDO) {
        TPlaylistDTO tPlaylistDTO;
        String json = logDO.getJson();
        List<JSONObject> playlistEvents = new ArrayList<>();
        if (StringUtils.isNotEmpty(json)) {
            tPlaylistDTO = JSON.parseObject(json, TPlaylistDTO.class);
            List<JSONObject> events = JSON
                    .parseArray(JSON.toJSONString(tPlaylistDTO.getEvents()), JSONObject.class);
            if (CollectionUtils.isNotEmpty(events)) {
                for (JSONObject event : events) {
                    String type = event.getString(PlaylistConst.PLAYLIST_EVENT_TYPE_KEY);
                    if (ContentTypeEnum.SEGMENT.equals(ContentTypeEnum.getByName(type))) {
                        String content_kind = event
                                .getString(PlaylistConst.PLAYLIST_EVENT_CONTENT_KIND_KEY);
                        if (SegmentTypeEnum.API_SEGMENT
                                .equals(SegmentTypeEnum.getByName(content_kind))) {
                            event.replace(PlaylistConst.PLAYLIST_EVENT_TYPE_KEY,
                                    PlaylistConst.PLAYLIST_EVENT_TYPE_PLACEHOLDER);
                            event.replace(PlaylistConst.PLAYLIST_EVENT_CONTENT_KIND_KEY, null);
                            removeRedundantKey(event);
                            playlistEvents.add(event);
                        } else {
                            String segmentEventStr = event
                                    .getString(PlaylistConst.PLAYLIST_EVENTS_KEY);
                            if (StringUtils.isNotEmpty(segmentEventStr)) {
                                List<JSONObject> segmentEvents = JSON
                                        .parseArray(segmentEventStr, JSONObject.class);
                                if (CollectionUtils.isNotEmpty(segmentEvents)) {
                                    segmentEvents.forEach(x -> {
                                        removeZeroAutomation(x);
                                        removeRedundantKey(x);
                                    });
                                    playlistEvents.addAll(segmentEvents);
                                }
                            }
                        }
                    } else {
                        removeZeroAutomation(event);
                        removeRedundantKey(event);
                        playlistEvents.add(event);
                    }
                }
            }
        } else {
            tPlaylistDTO = new TPlaylistDTO();
            tPlaylistDTO.setTitle(logDO.getTitle());
        }
        tPlaylistDTO.setEvents(playlistEvents);
        tPlaylistDTO.setUuid(logDO.getPlaylistUuid());
        tPlaylistDTO.setContentIds(JSON.parseArray(logDO.getContentIds(), String.class));
        // feedback logDO
        logDO.setJson(JSON.toJSONString(tPlaylistDTO));
        logDO.setTitle(tPlaylistDTO.getTitle());
        return tPlaylistDTO;
    }

    default void removeZeroAutomation(JSONObject event) {
        // check cpl automation,if seconds is empty(zero),then remove it
        String cplAutomation = event.getString(PlaylistConst.PLAYLIST_EVENT_AUTOMATION_KEY);
        if (StringUtils.isNotEmpty(cplAutomation)) {
            List<AutomationModel> cplAutomationModels = JSON
                    .parseArray(cplAutomation, AutomationModel.class);
            cplAutomationModels.removeIf(next -> next.getSpecific().getSeconds() == 0);
            event.replace(PlaylistConst.PLAYLIST_EVENT_AUTOMATION_KEY, cplAutomationModels);
        }
    }

    default void removeRedundantKey(JSONObject event) {
        event.remove(PlaylistConst.ORGANIZATION_ID_KEY);
        event.remove("title");
    }

    default TPlaylistHashSyncRequestDTO toPlaylistHashSyncRequest(String complexId, String deviceId,
            String playlistUuid) {
        TPlaylistHashSyncRequestDTO requestDTO = new TPlaylistHashSyncRequestDTO();
        requestDTO.setComplexId(complexId);
        requestDTO.setDeviceIds(Collections.singletonList(deviceId));
        requestDTO.setPlaylistIds(Collections.singletonList(playlistUuid));
        return requestDTO;
    }

    @Mappings({
            @Mapping(source = "complexId", target = "complexId"),
            @Mapping(source = "deviceId", target = "deviceId"),
            @Mapping(source = "playlistUuid", target = "playlistUuid"),
    })
    TPlaylistSyncRequestDTO toPlaylistSyncRequest(String complexId, String deviceId,
            String playlistUuid);

    @Mappings({
            @Mapping(source = "posUuid", target = "posUuid"),
            @Mapping(source = "tplUuid", target = "tplUuid"),
            @Mapping(source = "action", target = "mappingState")
    })
    PosMappingRequestDTO toPosMappingDTO(String posUuid, String tplUuid, String action);

    default PosMappingBatchRequestDTO toPosMappingBatchDTO(String complexUuid,
            List<PosMappingRequestDTO> mappings, String receiptUuid) {
        PosMappingBatchRequestDTO dto = new PosMappingBatchRequestDTO();
        dto.setReceiptUuid(receiptUuid == null ? UUID.randomUUID().toString() : receiptUuid);
        dto.setComplexUuid(complexUuid);
        dto.setMappings(mappings);
        return dto;
    }

    default TPlaylistDeletionDTO toTPlaylistDeletionDTO(String orgId, String complexUuid,
            String pplUuid, List<String> tplUUIDs) {
        TPlaylistDeletionDTO tPlaylistDeletionDTO = new TPlaylistDeletionDTO();
        tPlaylistDeletionDTO.setOrganizationId(orgId);
        tPlaylistDeletionDTO.setPlaylistUUIDs(tplUUIDs);
        tPlaylistDeletionDTO.setComplexUuid(complexUuid);
        tPlaylistDeletionDTO.setPplUuid(pplUuid);
        return tPlaylistDeletionDTO;
    }

    default PplContentsDataDTO toPplContentsDataDTO(String organizationId, String complexUuid,
            String pplUuid, Map<String, List<String>> tplContentsMap, String pplTitle) {
        PplContentsDataDTO pplContentsDataDTO = new PplContentsDataDTO();
        pplContentsDataDTO.setOrganizationId(organizationId);
        pplContentsDataDTO.setComplexUuid(complexUuid);
        pplContentsDataDTO.setPplUuid(pplUuid);
        pplContentsDataDTO.setPplTitle(pplTitle);
        pplContentsDataDTO.setTplContentsMap(tplContentsMap);
        return pplContentsDataDTO;
    }

    default PosFetchDTO toPosFetchDTO(String complexUuid, Integer week, List<String> postIds) {
        PosFetchDTO posFetchDTO = new PosFetchDTO();
        posFetchDTO.setComplexUuid(complexUuid);
        posFetchDTO.setWeek(week);
        posFetchDTO.setPosUuidList(postIds);
        return posFetchDTO;
    }

    default TPlaylistDeleteRequestDTO toTPlaylistDeleteRequest(String receiptUuid,
            String complexUuid, String deviceUuid, List<String> tplUUIDs) {
        TPlaylistDeleteRequestDTO dto = new TPlaylistDeleteRequestDTO();
        dto.setReceiptUuid(receiptUuid);
        dto.setComplexUuid(complexUuid);
        dto.setDeviceUuid(deviceUuid);
        dto.setTplUUIDs(tplUUIDs);
        return dto;
    }
}
