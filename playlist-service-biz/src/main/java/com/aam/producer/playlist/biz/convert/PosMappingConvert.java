package com.aam.producer.playlist.biz.convert;

import com.aam.producer.playlist.biz.enums.AAMSysEnum;
import com.aam.producer.playlist.biz.enums.PosMappingStatusEnum;
import com.aam.producer.playlist.protocol.message.PosMappingBatchRequestDTO;
import com.aam.producer.playlist.protocol.request.PosDataDTO;
import com.aam.producer.playlist.protocol.request.PosMappingDTO;
import com.aam.producer.playlist.protocol.request.PosMappingSentDTO;
import com.aam.producer.playlist.protocol.request.TPlaylistTransferDTO;
import com.aam.producer.playlist.repository.entity.PosMappingSendLogDO;
import com.aam.producer.playlist.repository.entity.PosPlaylistMappingDO;
import com.alibaba.fastjson.JSON;
import java.util.List;
import org.apache.commons.lang3.StringUtils;
import org.mapstruct.Mapper;
import org.mapstruct.factory.Mappers;

/**
 * com.aam.producer.playlist.biz.convert
 *
 * @author oliver.lo
 * @since 2019-09-18 16:55
 */
@Mapper
public interface PosMappingConvert {

    PosMappingConvert MAPPER = Mappers.getMapper(PosMappingConvert.class);

    default TPlaylistTransferDTO toPlaylistTransferDTO(String complexUuid, String deviceUuid,
            String playlistUuid, String posId, Boolean fastTrack) {
        TPlaylistTransferDTO dto = new TPlaylistTransferDTO();
        dto.setComplexUuid(complexUuid);
        dto.setDeviceUuid(deviceUuid);
        dto.setPlaylistUuid(playlistUuid);
        dto.setPosId(posId);
        dto.setFastTrack(fastTrack);
        return dto;
    }

    default void mappingInProducerFilling(PosPlaylistMappingDO mapping, String pplUuid,
            Boolean automatic) {
        mapping.setPplUuid(pplUuid);
        mapping.setPplAutomatic(automatic);
        mapping.setTplUuid(null);
        mapping.setAssignSystem(AAMSysEnum.PRODUCER.getCode());
        mapping.setMappingCompleted(false);
        mapping.setMappingStatus(PosMappingStatusEnum.MARK.getTitle());
        mapping.setMappingMessage(PosMappingStatusEnum.MARK.getTitle());
    }

    default void mappingFillingAfterTplChanged(PosPlaylistMappingDO mapping, String tplUuid) {
        mapping.setTplUuid(tplUuid);
        mapping.setMappingCompleted(false);
        mapping.setMappingStatus(PosMappingStatusEnum.MARK.getTitle());
        mapping.setMappingMessage(PosMappingStatusEnum.MARK.getTitle());
    }

    default void mappingInTmsFilling(PosPlaylistMappingDO mapping, String pplUuid,
            Boolean automatic, String tplUuid, String state, String message) {
        mapping.setPplUuid(pplUuid);
        mapping.setPplAutomatic(automatic);
        mapping.setTplUuid(tplUuid);
        mapping.setAssignSystem(AAMSysEnum.TMS.getCode());
        mapping.setMappingCompleted(true);
        mapping.setMappingStatus(state);
        mapping.setMappingMessage(message.length() < 200 ? message : message.substring(0, 200));
        // mapping.setAttempted(attempted); setting in newOrUpdatePosMapping
    }

    default void unMappingFilling(PosPlaylistMappingDO mapping, boolean confirm) {
        mapping.setPplUuid(StringUtils.EMPTY);
        mapping.setPplAutomatic(null);
        mapping.setTplUuid(null);
        mapping.setAssignSystem(confirm ? null : AAMSysEnum.PRODUCER.getCode());
        mapping.setMappingCompleted(confirm);
        mapping.setMappingStatus(confirm ? PosMappingStatusEnum.UNASSIGNED.getTitle()
                : PosMappingStatusEnum.MARK.getTitle());
        mapping.setMappingMessage(confirm ? PosMappingStatusEnum.UNASSIGNED.getTitle()
                : PosMappingStatusEnum.MARK.getTitle());
    }

    default void mappingConfirmFilling(PosPlaylistMappingDO mapping, String state, String message,
            boolean absolutely) {
        mapping.setMappingCompleted(true);
        if (absolutely) {
            mapping.setState(state);
            mapping.setMappingStatus(state);
            mapping.setMappingMessage(message.length() < 200 ? message : message.substring(0, 200));
        }
        // mapping.setAttempted(attempted); setting in newOrUpdatePosMapping
    }

    default PosPlaylistMappingDO newOrUpdatePosMapping(PosPlaylistMappingDO mapping, PosDataDTO pos,
            String orgId, Long attempted, Long start, Long end, Long showAttributeCode) {
        if (mapping == null) {
            mapping = new PosPlaylistMappingDO();
            mapping.setOrganizationId(orgId);
            mapping.setPosUuid(pos.getPosUuid());
            mapping.setPplUuid(StringUtils.EMPTY);
            mapping.setTplUuid(pos.getTplUuid());
        }

        mapping.setState(pos.getState());
        mapping.setPosTitle(pos.getTitle());
        mapping.setPosStart(start);
        mapping.setPosEnd(end);
        mapping.setComplexUuid(pos.getComplexUuid());
        mapping.setScreenUuid(pos.getScreenUuid());
        mapping.setTitleUuid(pos.getTitleUuid());
        mapping.setUnmatchedShowAttributes(JSON.toJSONString(pos.getUnmatchedShowAttributes()));
        mapping.setShowAttributes(JSON.toJSONString(pos.getShowAttributes()));
        mapping.setShowAttributesCode(
                showAttributeCode == null ? Long.valueOf(-1) : showAttributeCode);
        mapping.setLanguage(pos.getLanguage());

        mapping.setDeleted(pos.getDeleted());
        if (attempted != null) {
            mapping.setAttempted(attempted);
        }
        return mapping;
    }

    default void posDataFilling(PosDataDTO dto, String pplUuid, String tplUuid,
            String mappingSys, String state, Boolean automatic) {
        dto.setPplUuid(pplUuid);
        dto.setTplUuid(tplUuid);
        dto.setMappingInSystem(mappingSys);
        dto.setState(state);
        dto.setPplAutomatic(automatic);
    }

    default PosMappingDTO toPosManualMappingDTO(String pplUuid, String state,
            List<String> posUuidList, Boolean automatic, String mappingInSys) {
        PosMappingDTO posMappingDTO = new PosMappingDTO();
        posMappingDTO.setPplUuid(pplUuid);
        posMappingDTO.setPosUuidList(posUuidList);
        posMappingDTO.setState(state);
        posMappingDTO.setPplAutomatic(automatic);
        posMappingDTO.setMappingInSystem(mappingInSys);
        return posMappingDTO;
    }

    default PosMappingSendLogDO toPosMappingSendLogDO(PosMappingBatchRequestDTO requestDTO) {
        PosMappingSendLogDO posMappingSendLogDO = new PosMappingSendLogDO();
        posMappingSendLogDO.setReceiptUuid(requestDTO.getReceiptUuid());
        posMappingSendLogDO.setComplexId(requestDTO.getComplexUuid());
        posMappingSendLogDO.setMapping(JSON.toJSONString(requestDTO.getMappings()));
        posMappingSendLogDO.setStatus(PosMappingStatusEnum.REQUEST.getTitle());
        posMappingSendLogDO.setMessage(PosMappingStatusEnum.REQUEST.getTitle());
        return posMappingSendLogDO;
    }

    default void posMappingSendLogFilling(PosMappingSendLogDO logDO, PosMappingSentDTO dto,
            Long attempted) {
        logDO.setStatus(dto.getMappingStatus());
        logDO.setMessage(dto.getRespMessage());
        logDO.setAttempted(attempted);
    }

    default void invalidPosMappingSendLog(PosMappingSendLogDO logDO) {
        logDO.setStatus(PosMappingStatusEnum.INVALID.getTitle());
        logDO.setMessage(PosMappingStatusEnum.INVALID.getTitle());
    }
}
