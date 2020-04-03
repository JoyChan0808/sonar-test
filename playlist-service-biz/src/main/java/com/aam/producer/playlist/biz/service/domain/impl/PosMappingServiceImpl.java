package com.aam.producer.playlist.biz.service.domain.impl;

import com.aam.producer.playlist.biz.convert.PosMappingConvert;
import com.aam.producer.playlist.biz.convert.TPlaylistActionConvert;
import com.aam.producer.playlist.biz.enums.AAMSysEnum;
import com.aam.producer.playlist.biz.enums.PosMappingStatusEnum;
import com.aam.producer.playlist.biz.enums.ResultCodeEnum;
import com.aam.producer.playlist.biz.enums.TPlaylistTaskActionEnum;
import com.aam.producer.playlist.biz.event.CreateTplEvent;
import com.aam.producer.playlist.biz.service.IPlaylistService;
import com.aam.producer.playlist.biz.service.IPosPlaylistMappingService;
import com.aam.producer.playlist.biz.service.IShowAttributeService;
import com.aam.producer.playlist.biz.service.ITmsPlaylistService;
import com.aam.producer.playlist.biz.service.domain.IMessageQueueService;
import com.aam.producer.playlist.biz.service.domain.IPlaylistViewService;
import com.aam.producer.playlist.biz.service.domain.IPosMappingSendService;
import com.aam.producer.playlist.biz.service.domain.IPosMappingService;
import com.aam.producer.playlist.biz.service.domain.ITPlaylistSendService;
import com.aam.producer.playlist.biz.service.domain.ITPlaylistSyncService;
import com.aam.producer.playlist.biz.util.EventPublish;
import com.aam.producer.playlist.biz.util.OrgUtil;
import com.aam.producer.playlist.common.utils.TimeUtils;
import com.aam.producer.playlist.protocol.request.PosDataDTO;
import com.aam.producer.playlist.protocol.request.PosMappingDTO;
import com.aam.producer.playlist.protocol.request.TPlaylistTransferDTO;
import com.aam.producer.playlist.protocol.request.TitleDataDTO;
import com.aam.producer.playlist.protocol.response.TmsPlaylistInfo;
import com.aam.producer.playlist.repository.entity.PlaylistDO;
import com.aam.producer.playlist.repository.entity.PosPlaylistMappingDO;
import com.aam.producer.playlist.repository.entity.TmsPlaylistDO;
import com.aam.producer.playlist.sal.client.IComplexFacadeClient;
import com.aam.producer.playlist.sal.client.IViewFacadeClient;
import com.aam.utils.enums.BaseResultCode;
import com.aam.utils.exception.BizException;
import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.TypeReference;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * playlist assign service impl
 *
 * @author oliver.lo
 * @since 2019/7/2 9:27 AM
 */
@Service
public class PosMappingServiceImpl implements IPosMappingService {

    private static final Logger logger = LoggerFactory.getLogger(PosMappingServiceImpl.class);

    private final static long NOT_DONE_AND_TRY_AGAIN = 10 * 60 * 1000;

    private final IComplexFacadeClient complexFacadeClient;
    private final ITPlaylistSyncService playlistSyncService;
    private final ITPlaylistSendService playlistSendService;
    private final IPosPlaylistMappingService playlistMappingService;
    private final IPlaylistService playlistService;
    private final ITmsPlaylistService tmsPlaylistService;
    private final IShowAttributeService showAttributeService;
    private final IViewFacadeClient viewFacadeClient;
    private final IPosMappingSendService mappingSendService;
    private final IPlaylistViewService playlistViewService;
    private final EventPublish eventPublish;
    private final IMessageQueueService messageQueueService;

    @Autowired
    public PosMappingServiceImpl(
            IComplexFacadeClient complexFacadeClient,
            ITPlaylistSyncService playlistSyncService,
            ITPlaylistSendService playlistSendService,
            IPosPlaylistMappingService playlistMappingService,
            IPlaylistService playlistService,
            ITmsPlaylistService tmsPlaylistService,
            IShowAttributeService showAttributeService,
            IViewFacadeClient viewFacadeClient,
            IPosMappingSendService mappingSendService,
            IPlaylistViewService playlistViewService,
            EventPublish eventPublish,
            IMessageQueueService messageQueueService) {
        this.complexFacadeClient = complexFacadeClient;
        this.playlistSyncService = playlistSyncService;
        this.playlistSendService = playlistSendService;
        this.playlistMappingService = playlistMappingService;
        this.playlistService = playlistService;
        this.tmsPlaylistService = tmsPlaylistService;
        this.showAttributeService = showAttributeService;
        this.viewFacadeClient = viewFacadeClient;
        this.mappingSendService = mappingSendService;
        this.playlistViewService = playlistViewService;
        this.eventPublish = eventPublish;
        this.messageQueueService = messageQueueService;
    }

    @Override
    @Transactional
    public void posMappingManual(String pplUuid, Set<String> posUuidList) {
        if (StringUtils.isEmpty(pplUuid) || CollectionUtils.isEmpty(posUuidList)) {
            throw new BizException(BaseResultCode.METHOD_ARGUMENT_NOT_VALID);
        }

        PlaylistDO playlistDO = playlistService.getById(pplUuid);
        if (playlistDO == null) {
            logger.warn("pos try to mapping a not exist ppl,ppl_uuid:<{}>,org_id:<{}>",
                    pplUuid, OrgUtil.orgContenter.get());
            throw new BizException(ResultCodeEnum.NOT_FIND_PPL);
        }
        // get valid pos mapping
        // fixme maybe we should filter by ppl org?
        List<PosPlaylistMappingDO> posMappings = playlistMappingService
                .getMappingsByPos(true, new ArrayList<>(posUuidList));

        Set<String> posUUIDsInDB = posMappings.stream().map(PosPlaylistMappingDO::getPosUuid)
                .collect(Collectors.toSet());
        if (CollectionUtils.isEmpty(posUUIDsInDB) || posUUIDsInDB.size() != posUuidList.size()) {
            posUuidList.removeAll(posUUIDsInDB);
            logger.warn("pos manual mapping,but found invalid pos,ppl_uuid:<{}>,invalid_pos:<{}>",
                    pplUuid, posUuidList);
            throw new BizException(ResultCodeEnum.INVALID_POS);
        } else {
            logger.info("pos manual mapping,ppl_uuid:<{}>,finally mapping pos:<{}>", pplUuid,
                    posUUIDsInDB);
        }

        // handle
        handlePosMapping(playlistDO, posMappings, pplUuid, playlistDO.getAutomaticallyApply(),
                false);
    }

    @Override
    public void handlePplPublish(PlaylistDO playlistDO, List<Long> showAttributeCodes) {
        String pplUuid = playlistDO.getUuid();
        List<PosPlaylistMappingDO> posMappings;
        if (Boolean.TRUE.equals(playlistDO.getAutomaticallyApply())) {
            // get valid pos mapping by automatic show attribute
            posMappings = playlistMappingService
                    .getMappingsByAutoPpl(playlistDO.getOrganizationId(), true, true,
                            showAttributeCodes, playlistDO.getUuid());
        } else {
            // get valid pos mapping by static ppl uuid
            posMappings = playlistMappingService.getMappingsByPpl(true, pplUuid);
        }

        logger.info(
                "ppl publish,ppl:<{}>,automatic:<{}>,show_attribute_codes:<{}>,finally mapping pos:<{}>",
                playlistDO.toString(), playlistDO.getAutomaticallyApply(), showAttributeCodes,
                posMappings.stream().map(PosPlaylistMappingDO::getPosUuid)
                        .collect(Collectors.toList()));
        // handle
        handlePosMapping(playlistDO, posMappings, pplUuid, playlistDO.getAutomaticallyApply(),
                true);
    }

    @Override
    public void posUnMapping(String pplUuid) {
        // get valid pos mapping
        List<PosPlaylistMappingDO> posMappings = playlistMappingService
                .getMappingsByPpl(true, pplUuid);

        logger.info("pos un-mapping,ppl_uuid:<{}>,finally un-mapping pos_uuid:<{}>",
                pplUuid, posMappings.stream().map(PosPlaylistMappingDO::getPosUuid)
                        .collect(Collectors.toList()));
        // handle
        handlePosUnMapping(posMappings);
    }

    @Override
    public TmsPlaylistInfo reviewMappedTpl(String pplUuid, String posUuid) {
        PosPlaylistMappingDO mapping = playlistMappingService.getOneMapping(posUuid);
        if (mapping == null) {
            logger.warn("pos review tpl,but pos is not exist,ppl_uuid:<{}>,pos_uuid:<{}>",
                    pplUuid, posUuid);
            throw new BizException(ResultCodeEnum.POS_NOT_EXIST_OR_DELETED);
        }
        if (StringUtils.isNotEmpty(mapping.getTplUuid())) {
            QueryWrapper<TmsPlaylistDO> wrapper = new QueryWrapper<>();
            wrapper.lambda().eq(TmsPlaylistDO::getSourcePplId, pplUuid)
                    .eq(TmsPlaylistDO::getPlaylistUuid, mapping.getTplUuid())
                    .eq(TmsPlaylistDO::getOrganizationId, mapping.getOrganizationId());
            TmsPlaylistDO one = tmsPlaylistService.getOne(wrapper);
            if (one != null) {
                PlaylistDO playlistDO = playlistService.getByIdIgnoreOrgId(pplUuid);
                TmsPlaylistInfo tplInfo = TPlaylistActionConvert.MAPPER
                        .toPlaylistInfo(one, playlistDO == null ? null : playlistDO.getTitle());
                if (mapping.getAssignSystem() != null && mapping.getAssignSystem() == AAMSysEnum.TMS
                        .getCode()) {
                    // set format to cpl in tpl
                    TPlaylistActionConvert.MAPPER.setPro2CplKey(tplInfo);
                }
                return tplInfo;
            } else {
                logger.warn(
                        "pos review tpl,but tpl is not sync back yet,ppl_uuid:<{}>,pos_uuid:<{}>",
                        pplUuid, posUuid);
                throw new BizException(ResultCodeEnum.UNFINISHED_PLAYLIST_SYNCHRONIZATION);
            }
        } else {
            logger.warn("pos review tpl,but pos is not match tpl yet,ppl_uuid:<{}>,pos_uuid:<{}>",
                    pplUuid, posUuid);
            throw new BizException(ResultCodeEnum.UNFINISHED_POS_MAPPING);
        }
    }

    @Override
    public void handlePosMappedTpl(Map<String, String> posTplMap, List<TmsPlaylistDO> tplList) {
        if (MapUtils.isNotEmpty(posTplMap)) {
            // get valid pos mapping
            List<PosPlaylistMappingDO> mappings = playlistMappingService
                    .getMappingsByPos(false, new ArrayList<>(posTplMap.keySet()));
            if (CollectionUtils.isNotEmpty(mappings)) {
                // save tpl in pos mapping
                mappings.forEach(x -> PosMappingConvert.MAPPER
                        .mappingFillingAfterTplChanged(x, posTplMap.get(x.getPosUuid())));
                playlistMappingService.updateBatchByOrderIds(mappings);

                // send tpl to complex
                Map<String, TmsPlaylistDO> tplMap = tplList.stream().collect(
                        Collectors.toMap(TmsPlaylistDO::getPlaylistUuid, Function.identity()));
                Map<String, Set<String>> tplComplexesMap = mappings.stream().collect(Collectors
                        .groupingBy(PosPlaylistMappingDO::getTplUuid, Collectors
                                .mapping(PosPlaylistMappingDO::getComplexUuid,
                                        Collectors.toSet())));
                tplComplexesMap.forEach((tplUuid, complexes) -> {
                    TmsPlaylistDO tpl = tplMap.get(tplUuid);
                    if (tpl != null) {
                        for (String complexUuid : complexes) {
                            sendTplToTms(complexUuid, tplUuid, tpl);
                        }
                    } else {
                        logger.warn(
                                "pos match tpl,but tpl found not in db,tpl_uuid:<{}>,pos_tpl_map:<{}>",
                                tplUuid, posTplMap);
                        throw new BizException(ResultCodeEnum.NOT_FIND_TPL);
                    }
                });

                // send pos tpl matched data
                Map<String, List<String>> tplPosMap = new HashMap<>();
                posTplMap.forEach((k, v) -> {
                    List<String> orDefault = tplPosMap.getOrDefault(v, new ArrayList<>());
                    orDefault.add(k);
                    tplPosMap.putIfAbsent(v, orDefault);
                });
                messageQueueService.tplPosMatchedData(tplPosMap);
            }
        }
    }

    @Override
    public boolean handlePosData(PosDataDTO posData) {
        boolean needMore = false;

        // set org id
        String orgId = complexFacadeClient.getOrganizationUuid(posData.getComplexUuid());
        if (StringUtils.isEmpty(orgId)) {
            throw new BizException(ResultCodeEnum.ORGANIZATION_NOT_FIND);
        }
        OrgUtil.orgContenter.set(orgId);

        // init
        String tplUuid = posData.getTplUuid();
        String posUuid = posData.getPosUuid();
        PosPlaylistMappingDO mapping = playlistMappingService.getOneMapping(posUuid);
        Long attempted = TimeUtils.changeMillisecondTimestamp(posData.getPosLastModified());

        // expired pos.data
        if (mapping != null && mapping.getAttempted() != null && attempted != null
                && mapping.getAttempted() > attempted) {
            logger.warn(
                    "pos.data pos last modified is expired,pos_uuid:<{}>,pos_attempted:<{}>,mapping_attempted:<{}>",
                    posData.getPosUuid(), attempted, mapping.getAttempted());
            mapping.setAttempted(attempted);
            playlistMappingService.updateById(mapping);
            return true;
        }

        Long start = TimeUtils.changeMillisecondTimestamp(posData.getStart());
        Long end = TimeUtils.changeMillisecondTimestamp(posData.getEnd());
        Long showAttributeCode = showAttributeCode(posData.getShowAttributes());

        // factor changed?
        boolean factorChanged = factorChanged(posData, mapping, start, end, showAttributeCode);
        // show attribute changed?
        boolean showAttributeCodeChanged = showAttributeCodeChanged(mapping, showAttributeCode);
        // filling mapping by pos.data
        mapping = newOrUpdatePosMapping(posData, mapping, attempted, start, end, showAttributeCode);
        // mapping sys
        AAMSysEnum mappingSysEnum = AAMSysEnum.getByCode(mapping.getAssignSystem());
        boolean needToMapTpl = false;

        if (AAMSysEnum.TMS.equals(mappingSysEnum)) {
            if (invalidAssigned(tplUuid, posData.getState())) {
                logger.info(
                        "[unassigned in tms] pos.data try to automatic mapping ppl for pos which assigned in tms before,pos_uuid:<{}>",
                        posUuid);
                PosMappingConvert.MAPPER
                        .unMappingFilling(mapping, true);
                // try to automatic mapping in producer
                mappingPplForPos(mapping, false);
                needToMapTpl = true;
            } else {
                if (Objects.equals(mapping.getTplUuid(), posData.getTplUuid())) {
                    logger.info("[confirm] pos.data pos assigned in tms confirm,pos_uuid:<{}>",
                            posUuid);
                    PosMappingConvert.MAPPER
                            .mappingConfirmFilling(mapping, posData.getState(),
                                    posData.getMessage(), true);
                } else {
                    posMappingCoveredByTms(posData, mapping);
                }
            }
        } else if (AAMSysEnum.PRODUCER.equals(mappingSysEnum)) {
            if (factorChanged && Boolean.TRUE.equals(mapping.getPplAutomatic())) {
                logger.info(
                        "[factor changed] pos.data try to rematch tpl for pos because factor changed,pos_uuid:<{}>",
                        posUuid);
                needToMapTpl = true;
                if (showAttributeCodeChanged || StringUtils.isEmpty(mapping.getTitleUuid())) {
                    logger.info(
                            "[show attribute changed/title is gone] pos.data try to automatic mapping ppl for pos again,pos_uuid:<{}>",
                            posUuid);
                    mappingPplForPos(mapping, true);
                }

            } else if (PosMappingStatusEnum.MARK.getTitle().equals(mapping.getMappingStatus())) {
                logger.info(
                        "[protecting mapping] pos.data mapping have not sending to tms yet,pos_uuid:<{}>",
                        posUuid);
                if (StringUtils.isNotEmpty(mapping.getPplUuid()) && StringUtils
                        .isEmpty(mapping.getTplUuid())) {
                    needToMapTpl = true;
                }
                if (StringUtils.isEmpty(mapping.getPplUuid()) && invalidAssigned(tplUuid,
                        posData.getState())) {
                    PosMappingConvert.MAPPER.mappingConfirmFilling(mapping, posData.getState(),
                            posData.getMessage(), true);
                }
            } else if (invalidAssigned(tplUuid, posData.getState())) {
                if (pro2MappingProtecting(mapping.getLastModified())) {
                    throw new BizException(ResultCodeEnum.POS_DATA_UNRELIABLE);
                }

                logger.info(
                        "[unassigned in tms] pos.data try to automatic mapping ppl for pos which un-mapping in tms,pos_uuid:<{}>",
                        posUuid);
                PosMappingConvert.MAPPER
                        .unMappingFilling(mapping, true);
                // try to automatic mapping in producer
                mappingPplForPos(mapping, true);
                needToMapTpl = true;
            } else {
                if (Objects.equals(mapping.getTplUuid(), posData.getTplUuid())) {
                    logger.info(
                            "[confirm] pos.data pos mapping in producer confirm,pos_uuid:<{}>",
                            posUuid);
                    PosMappingConvert.MAPPER
                            .mappingConfirmFilling(mapping, posData.getState(),
                                    posData.getMessage(), true);
                } else {
                    if (attempted == null && Boolean.FALSE.equals(mapping.getMappingCompleted())) {
                        // doubt for confirm
                        PosMappingConvert.MAPPER.mappingConfirmFilling(mapping, posData.getState(),
                                posData.getMessage(), false);
                        // need to fetch pos
                        needMore = true;
                        logger.warn(
                                "[doubt] pos.data mapping maybe covered by tms or expired data without attempted time,pos_uuid:<{}>,need more pos.fetch",
                                posData.getPosUuid());
                    } else {
                        if (StringUtils.isNotEmpty(mapping.getTplUuid())) {
                            // note: if pos assigned in pro,can't be covered in tms,resend to tms
                            mappingSendService
                                    .handleMappingSendRequest(Collections.singletonList(mapping),
                                            PosMappingStatusEnum.ASSIGNED.getTitle());
                            logger.warn(
                                    "[resend mapping] pos.data mapping is incorrect,try sending to tms again,pos_uuid:<{}>",
                                    posUuid);
                        } else {
                            posMappingCoveredByTms(posData, mapping);
                        }
                    }
                }
            }
        } else {
            if (invalidAssigned(tplUuid, posData.getState())) {
                logger.info(
                        "[unassigned in tms] pos.data try to automatic mapping ppl for pos which not assigned before,pos_uuid:<{}>",
                        posUuid);
                PosMappingConvert.MAPPER
                        .unMappingFilling(mapping, true);
                // try to automatic mapping in producer
                mappingPplForPos(mapping, false);
                needToMapTpl = true;
            } else {
                posMappingCoveredByTms(posData, mapping);
            }
        }

        // pos match tpl
        String pplUuid = mapping.getPplUuid();
        if (needToMapTpl && StringUtils.isNotEmpty(pplUuid)) {
            PlaylistDO playlistDO = playlistService.getById(pplUuid);
            if (playlistDO == null) {
                logger.warn("pos.data pos try to mapping a not exist ppl,ppl_uuid:<{}>,org_id:<{}>",
                        pplUuid, orgId);
                PosMappingConvert.MAPPER
                        .unMappingFilling(mapping, true);
                // save or update pos mapping
                playlistMappingService.saveOrUpdate(mapping);
            } else {
                // save or update pos mapping
                playlistMappingService.saveOrUpdate(mapping);
                try {
                    eventPublish.publishEvent(
                            new CreateTplEvent(playlistDO, Collections.singletonList(mapping),
                                    TPlaylistTaskActionEnum.MATCH.getCode()));
                } catch (BizException e) {
                    logger.error(
                            "pos.data pos try to map tpl error:<{}>,no more pos.fetch,pos_uuid:<{}>",
                            e.getMessage(), posUuid, e);
                } catch (Exception e) {
                    logger.error(
                            "pos.data pos try to map tpl error:<{}>,need more pos.fetch,pos_uuid:<{}>",
                            e.getMessage(), posUuid, e);
                    needMore = true;
                }
            }
        } else {
            // save or update pos mapping
            playlistMappingService.saveOrUpdate(mapping);
        }

        // pos-match.data filling
        mappingSysEnum = AAMSysEnum.getByCode(mapping.getAssignSystem());
        PosMappingConvert.MAPPER
                .posDataFilling(posData, pplUuid, mapping.getTplUuid(),
                        mappingSysEnum == null ? null : mappingSysEnum.name(),
                        PosMappingStatusEnum
                                .titleForView(mapping.getMappingStatus(), pplUuid),
                        mapping.getPplAutomatic());

        return needMore;
    }

    @Override
    public void handleTitleData(TitleDataDTO titleData) {
        List<PosPlaylistMappingDO> mappings = playlistMappingService
                .getMappingByTitle(true, titleData.getTitleUuid());
        Map<String, List<PosPlaylistMappingDO>> pplPosMap = mappings.stream()
                .filter(x -> StringUtils.isNotEmpty(x.getPplUuid()) && Boolean.TRUE
                        .equals(x.getPplAutomatic()))
                .collect(Collectors
                        .groupingBy(PosPlaylistMappingDO::getPplUuid, Collectors.toList()));
        pplPosMap.forEach((x, y) -> {
            // fixme get ppl without org_id
            PlaylistDO playlistDO = playlistService.getByIdIgnoreOrgId(x);
            if (playlistDO != null) {
                OrgUtil.orgContenter.set(playlistDO.getOrganizationId());
                eventPublish
                        .publishEvent(
                                new CreateTplEvent(playlistDO, y,
                                        TPlaylistTaskActionEnum.NEW_CONTENT.getCode()));
            }
        });
    }

    private boolean invalidAssigned(String tplUuid, String state) {
        return StringUtils.isEmpty(tplUuid) || PosMappingStatusEnum.UNASSIGNED.getTitle()
                .equals(state);
    }

    private void mappingPplForPos(PosPlaylistMappingDO mapping, boolean mappedInProBefore) {
        // do not change manual mapping
        if (Boolean.FALSE.equals(mapping.getPplAutomatic())) {
            logger.info("pos.data manual matched and don't change ppl automatic");
            return;
        }

        // un-mapping pos without title uuid
        if (StringUtils.isEmpty(mapping.getTitleUuid())) {
            logger.info("pos.data un-mapping because title uuid is null,pos_uuid:<{}>",
                    mapping.getPosUuid());
            // un-mapping pos which no title uuid
            PosMappingConvert.MAPPER.unMappingFilling(mapping, !mappedInProBefore);
            if (mappedInProBefore) {
                // send pos un-assign to tms
                mappingSendService.handleMappingSendRequest(Collections.singletonList(mapping),
                        PosMappingStatusEnum.UNASSIGNED.getTitle());
            }
            return;
        }

        PlaylistDO ppl = null;
        if (mapping.getShowAttributesCode() != null) {
            // show attribute code automatic mapping ppl
            ppl = playlistViewService.getPlaylist(mapping.getShowAttributesCode());
        }

        logger.info("pos.data automatic mapped ppl:<{}>", ppl == null ? null : ppl.getUuid());
        if (ppl != null) {
            PosMappingConvert.MAPPER.mappingInProducerFilling(mapping, ppl.getUuid(),
                    ppl.getAutomaticallyApply());
        } else {
            PosMappingConvert.MAPPER.unMappingFilling(mapping, !mappedInProBefore);
            if (mappedInProBefore) {
                // send pos un-assign to tms
                mappingSendService
                        .handleMappingSendRequest(Collections.singletonList(mapping),
                                PosMappingStatusEnum.UNASSIGNED.getTitle());
            }
        }
    }

    private void posMappingCoveredByTms(PosDataDTO posData, PosPlaylistMappingDO mapping) {
        PosMappingConvert.MAPPER
                .mappingInTmsFilling(mapping, posData.getTplUuid(), null, posData.getTplUuid(),
                        posData.getState(), posData.getMessage());

        TmsPlaylistDO inPro = tmsPlaylistService.getOneTplScopeInPro(posData.getTplUuid());
        if (inPro == null) {
            logger.info(
                    "[assigned in tms] pos.data pos assigned tpl(create in tms) in tms,pos_uuid:<{}>",
                    mapping.getPosUuid());
            pullTplFromTms(posData.getComplexUuid(), posData.getPosUuid(), posData.getTplUuid());
        } else {
            logger.info(
                    "[assigned in tms] pos.data pos assigned tpl(create in pro) in tms,pos_uuid:<{}>",
                    mapping.getPosUuid());
        }
    }

    private void sendTplToTms(String complexUuid, String tplUuid, TmsPlaylistDO toBeSend) {
        // send playlist to tms
        String pplUuid = toBeSend.getSourcePplId();
        String lmsUuid = complexFacadeClient.getLmsUuid(complexUuid);
        TPlaylistTransferDTO transferDTO = PosMappingConvert.MAPPER
                .toPlaylistTransferDTO(complexUuid, lmsUuid, tplUuid, null, false);
        try {
            playlistSendService.handlePlaylistSendRequest(transferDTO, toBeSend);
        } catch (BizException e) {
            // tpl not found
            logger.warn(
                    "send tpl to complex,but tpl not found,rematch tpl for pos,bizCode[570]:<{}>",
                    e.getCode().getCode());
            if (e.getCode().getCode() == ResultCodeEnum.NOT_FIND_TPL.getCode()) {
                PlaylistDO playlistDO = playlistService.getById(pplUuid);
                if (playlistDO == null) {
                    List<PosPlaylistMappingDO> mappings = playlistMappingService
                            .getMappingsByPpl(true, pplUuid);
                    handlePosUnMapping(mappings);
                } else {
                    List<PosPlaylistMappingDO> mappings = playlistMappingService
                            .getMappingByTpl(pplUuid, tplUuid, null);
                    if (CollectionUtils.isNotEmpty(mappings)) {
                        eventPublish.publishEvent(new CreateTplEvent(playlistDO, mappings,
                                TPlaylistTaskActionEnum.MATCH.getCode()));
                    }
                }
            }
        }
    }

    private void pullTplFromTms(String complexUuid, String posUuid, String tplUuid) {
        // pull playlist from tms
        String lmsUuid = complexFacadeClient.getLmsUuid(complexUuid);
        TPlaylistTransferDTO transferDTO = PosMappingConvert.MAPPER
                .toPlaylistTransferDTO(complexUuid, lmsUuid, tplUuid, posUuid, false);
        playlistSyncService.handlePlaylistHashRequest(transferDTO);
    }

    private void handlePosMapping(PlaylistDO ppl, List<PosPlaylistMappingDO> posMappings,
            String pplUuid, Boolean automatic, boolean publish) {
        if (CollectionUtils.isNotEmpty(posMappings)) {
            // save ppl uuid to pos mapping
            List<String> posMappedIds = new ArrayList<>();
            posMappings.forEach(x -> {
                PosMappingConvert.MAPPER
                        .mappingInProducerFilling(x, pplUuid, automatic);
                posMappedIds.add(x.getPosUuid());
            });

            playlistMappingService.updateBatchByOrderIds(posMappings);

            // write to producer view
            PosMappingDTO posMappingDTO = PosMappingConvert.MAPPER
                    .toPosManualMappingDTO(pplUuid, PosMappingStatusEnum.PENDING.getTitle(),
                            posMappedIds, automatic, AAMSysEnum.PRODUCER.name());
            viewFacadeClient.sendPosMapping(posMappingDTO);
        }

        if (CollectionUtils.isNotEmpty(posMappings)) {
            // create tpl and pos match tpl
            try {
                if (publish) {
                    eventPublish.publishEvent(new CreateTplEvent(ppl, posMappings,
                            TPlaylistTaskActionEnum.PUBLISH.getCode()));
                } else {
                    eventPublish.publishEvent(new CreateTplEvent(ppl, posMappings,
                            TPlaylistTaskActionEnum.MATCH.getCode()));
                }
            } catch (Exception e) {
                logger.error("pos mapping ppl success,try to map tpl error:<{}>", e.getMessage(),
                        e);

                if (CollectionUtils.isNotEmpty(posMappings)) {
                    List<String> posMappedIds = posMappings.stream()
                            .map(PosPlaylistMappingDO::getPosUuid).collect(Collectors.toList());
                    // write to producer view
                    PosMappingDTO posMappingDTO = PosMappingConvert.MAPPER
                            .toPosManualMappingDTO(null, PosMappingStatusEnum.UNASSIGNED.getTitle(),
                                    posMappedIds, null, null);
                    viewFacadeClient.sendPosMapping(posMappingDTO);
                }

                throw new BizException(ResultCodeEnum.POS_MATCH_TPL_FAILED);
            }
        }
    }

    private void handlePosUnMapping(List<PosPlaylistMappingDO> posMappings) {
        if (CollectionUtils.isNotEmpty(posMappings)) {
            List<String> posMappedIds = new ArrayList<>();
            posMappings.forEach(x -> {
                PosMappingConvert.MAPPER.unMappingFilling(x, false);
                posMappedIds.add(x.getPosUuid());
            });
            playlistMappingService.updateBatchByOrderIds(posMappings);

            // write to producer view
            PosMappingDTO posMappingDTO = PosMappingConvert.MAPPER
                    .toPosManualMappingDTO(null, PosMappingStatusEnum.UNASSIGNED.getTitle(),
                            posMappedIds, null, null);
            viewFacadeClient.sendPosMapping(posMappingDTO);

            // send pos un-assign to tms
            mappingSendService
                    .handleMappingSendRequest(posMappings,
                            PosMappingStatusEnum.UNASSIGNED.getTitle());
        }
    }

    /**
     * new or update mapping,return factor changed or not
     *
     * @param posData pos
     * @param mapping mapping
     * @param showAttributeCode show attribute code
     * @return boolean
     */
    private PosPlaylistMappingDO newOrUpdatePosMapping(PosDataDTO posData,
            PosPlaylistMappingDO mapping, Long attempted, Long start, Long end,
            Long showAttributeCode) {
        String orgId = OrgUtil.orgContenter.get();
        return PosMappingConvert.MAPPER
                .newOrUpdatePosMapping(mapping, posData, orgId, attempted, start, end,
                        showAttributeCode);
    }

    private boolean factorChanged(PosDataDTO posData, PosPlaylistMappingDO mapping,
            Long start, Long end, Long showAttributeCode) {
        if (mapping == null) {
            return true;
        }
        Map<String, String> currentUnmatched = JSON
                .parseObject(mapping.getUnmatchedShowAttributes(),
                        new TypeReference<Map<String, String>>() {
                        });
        return !Objects.equals(mapping.getPosStart(), start)
                || !Objects.equals(mapping.getPosEnd(), end)
                || !Objects.equals(mapping.getComplexUuid(), posData.getComplexUuid())
                || !Objects.equals(mapping.getScreenUuid(), posData.getScreenUuid())
                || !Objects.equals(mapping.getTitleUuid(), posData.getTitleUuid())
                || !Objects.equals(currentUnmatched, posData.getUnmatchedShowAttributes())
                || !Objects.equals(mapping.getShowAttributesCode(), showAttributeCode)
                || !Objects.equals(mapping.getLanguage(), posData.getLanguage());
    }

    private Long showAttributeCode(Map<String, String> showAttribute) {
        if (MapUtils.isEmpty(showAttribute)) {
            return null;
        }
        return showAttributeService
                .getShortCodeSumByTitles(new ArrayList<>(showAttribute.values()));
    }

    private boolean showAttributeCodeChanged(PosPlaylistMappingDO mapping, Long ne) {
        return mapping == null
                || !Objects.equals(mapping.getShowAttributesCode(), ne);
    }

    private boolean pro2MappingProtecting(Long lastModified) {
        long now = System.currentTimeMillis();
        return (now - lastModified) < NOT_DONE_AND_TRY_AGAIN;
    }
}
