package com.aam.producer.playlist.biz.service.domain.impl;

import com.aam.producer.lib.enums.PlaylistSourceEnum;
import com.aam.producer.playlist.biz.convert.MessageQueueConvert;
import com.aam.producer.playlist.biz.convert.TPlaylistActionConvert;
import com.aam.producer.playlist.biz.enums.ResultCodeEnum;
import com.aam.producer.playlist.biz.model.PlaylistPublishModel;
import com.aam.producer.playlist.biz.model.PlaylistPublishModel.SegmentSplit;
import com.aam.producer.playlist.biz.service.IPosPlaylistMappingService;
import com.aam.producer.playlist.biz.service.ITmsPlaylistService;
import com.aam.producer.playlist.biz.service.domain.IMessageQueueService;
import com.aam.producer.playlist.biz.service.domain.IPosMappingService;
import com.aam.producer.playlist.biz.service.domain.ITPlaylistActionService;
import com.aam.producer.playlist.biz.service.domain.ITPlaylistSendService;
import com.aam.producer.playlist.biz.service.domain.ITPlaylistWarningService;
import com.aam.producer.playlist.protocol.message.PplContentsDataDTO;
import com.aam.producer.playlist.protocol.message.TPlaylistDeletionDTO;
import com.aam.producer.playlist.protocol.response.TmsPlaylistInfo;
import com.aam.producer.playlist.repository.entity.PosPlaylistMappingDO;
import com.aam.producer.playlist.repository.entity.TmsPlaylistDO;
import com.aam.utils.exception.BizException;
import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.TypeReference;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * playlist action service impl
 *
 * @author oliver.lo
 * @since 2019/7/8 6:41 PM
 */
@Service
public class TPlaylistActionServiceImpl implements ITPlaylistActionService {

    private static final Logger logger = LoggerFactory.getLogger(TPlaylistActionServiceImpl.class);

    private final ITmsPlaylistService tmsPlaylistService;
    private final IPosPlaylistMappingService playlistMappingService;
    private final ITPlaylistSendService playlistSendService;
    private final IPosMappingService posMappingService;
    private final IMessageQueueService mqService;
    private final ITPlaylistWarningService warningService;

    @Autowired
    public TPlaylistActionServiceImpl(
            ITmsPlaylistService tmsPlaylistService,
            IPosPlaylistMappingService playlistMappingService,
            ITPlaylistSendService playlistSendService,
            IPosMappingService posMappingService,
            IMessageQueueService mqService,
            ITPlaylistWarningService warningService) {
        this.tmsPlaylistService = tmsPlaylistService;
        this.playlistMappingService = playlistMappingService;
        this.playlistSendService = playlistSendService;
        this.posMappingService = posMappingService;
        this.mqService = mqService;
        this.warningService = warningService;
    }

    @Override
    public TmsPlaylistInfo getByPlaylistUuid(String playlistUuid, String complexId) {
        QueryWrapper<TmsPlaylistDO> wrapper = new QueryWrapper<>();
        wrapper.lambda().eq(TmsPlaylistDO::getPlaylistUuid, playlistUuid)
                .eq(TmsPlaylistDO::getSourceComplexId, complexId);
        TmsPlaylistDO one = tmsPlaylistService.getOne(wrapper);
        if (one == null) {
            throw new BizException(ResultCodeEnum.NOT_FIND_TPL);
        }
        TmsPlaylistInfo info = TPlaylistActionConvert.MAPPER.toPlaylistInfo(one, null);
        TPlaylistActionConvert.MAPPER.setPro2CplKey(info);
        return info;
    }

    @Override
    @Transactional
    public void handleTplPublish(PlaylistPublishModel model) {
        logger.info("handle playlist publish,playlist:<{}>", model);

        List<TmsPlaylistDO> oldVersions = this.tmsPlaylistService
                .getTplListScopeInPro(model.getPplUuid(), null);

        // delete un-publish tpl & protect tpl for some locked pos
        List<TmsPlaylistDO> needToDelete = oldVersions.stream()
                .filter(x -> !x.getSourcePplVersionId().equals(model.getPplVersionUuid())).collect(
                        Collectors.toList());
        if (CollectionUtils.isNotEmpty(needToDelete)) {
            removeAndProtectTpl(model, needToDelete);
            oldVersions.removeAll(needToDelete);
        }

        Map<String, TmsPlaylistDO> uniqueOldMap = getUniqueIdMap(oldVersions);
        Map<String, String> posTplMap = genPosTplMap(model.getPosUuidList());

        tplControlCenter(model, uniqueOldMap, posTplMap, true, true, true, true);
    }

    @Override
    @Transactional
    public void handleTplUnPublish(PlaylistPublishModel dto) {
        logger.info("handle playlist un-publish,playlist:<{}>", dto);

        List<TmsPlaylistDO> tpLsComeFromPro = tmsPlaylistService
                .getTplListScopeInPro(dto.getPplUuid(), null);
        if (CollectionUtils.isNotEmpty(tpLsComeFromPro)) {
            // protect tpl for some locked shows
            removeAndProtectTpl(dto, tpLsComeFromPro);
        }
    }

    @Override
    @Transactional
    public void handleTplTitleChange(PlaylistPublishModel model) {
        logger.info("handle playlist change title,playlist:<{}>", model);

        List<TmsPlaylistDO> oldVersions = tmsPlaylistService
                .getTplListScopeInPro(model.getPplUuid(), null);
        Map<String, TmsPlaylistDO> uniqueOldMap = getUniqueIdMap(oldVersions);

        Map<String, String> posTplMap = genPosTplMap(model.getPosUuidList());

        tplControlCenter(model, uniqueOldMap, posTplMap, false, true, false, false);
    }

    @Override
    @Transactional
    public void handlePosMatch(PlaylistPublishModel model) {
        logger.info("handle pos match,playlist:<{}>", model);

        List<TmsPlaylistDO> oldVersions = this.tmsPlaylistService
                .getTplListScopeInPro(model.getPplUuid(), null);
        Map<String, TmsPlaylistDO> uniqueOldMap = getUniqueIdMap(oldVersions);

        Map<String, String> posTplMap = genPosTplMap(model.getPosUuidList());

        tplControlCenter(model, uniqueOldMap, posTplMap, true, false, true, true);
    }

    private void tplControlCenter(PlaylistPublishModel model,
            Map<String, TmsPlaylistDO> uniqueOldMap, Map<String, String> posTplMap, boolean saveNew,
            boolean saveChanged, boolean sendPplContentData, boolean matchedPos) {
        List<TmsPlaylistDO> tplList = TPlaylistActionConvert.MAPPER
                .toTmsPlaylistDOFromPublish(model, posTplMap, uniqueOldMap);
        List<TmsPlaylistDO> pplContentList = new ArrayList<>();
        if (saveNew) {
            List<TmsPlaylistDO> newTplList = tplList.stream().filter(x -> x.getId() == null)
                    .collect(Collectors.toList());
            if (CollectionUtils.isNotEmpty(newTplList)) {
                // add to ppl-contents
                pplContentList.addAll(newTplList);

                // batch save new tpl
                tmsPlaylistService.saveBatch(newTplList);
            }
        }
        if (saveChanged) {
            List<TmsPlaylistDO> changedTplList = tplList.stream().filter(x -> x.getId() != null)
                    .sorted(Comparator.comparingLong(TmsPlaylistDO::getId)).collect(
                            Collectors.toList());
            if (CollectionUtils.isNotEmpty(changedTplList)) {
                // add to ppl-contents
                pplContentList.addAll(changedTplList);

                // batch save changed tpl
                tmsPlaylistService.updateBatchById(changedTplList);

                // send changed to sw
                changedTplList.forEach(playlistSendService::handlePlaylistChanged);
            }
        }
        if (sendPplContentData) {
            // send ppl-contents.data
            sendTplContentsData(model, pplContentList);
            // warning
            warningService.verifyTyl(model.getPplUuid(), model.getTitle(), pplContentList);
        }
        if (matchedPos) {
            // pos assign tpl
            posMappingService.handlePosMappedTpl(posTplMap, tplList);
        }
    }

    private void sendTplContentsData(PlaylistPublishModel model,
            List<TmsPlaylistDO> currentVersions) {
        if (CollectionUtils.isNotEmpty(currentVersions)) {
            Map<String, List<String>> tplContentMap = new HashMap<>(
                    (int) ((float) currentVersions.size() / 0.75F + 1.0F));
            currentVersions.forEach(x -> tplContentMap
                    .put(x.getPlaylistUuid(), JSON.parseArray(x.getContentIds(), String.class)));
            PplContentsDataDTO pplContentsDataDTO = MessageQueueConvert.MAPPER
                    .toPplContentsDataDTO(model.getOrganizationId(), null, model.getPplUuid(),
                            tplContentMap, model.getTitle());
            mqService.pplContentsData(pplContentsDataDTO);
        }
    }

    private void removeAndProtectTpl(PlaylistPublishModel dto, List<TmsPlaylistDO> oldVersions) {
        List<TmsPlaylistDO> needToDeleteTplList = new ArrayList<>();
        List<PosPlaylistMappingDO> protectedMappings = this.playlistMappingService
                .getProtectedMappings(dto.getPplUuid());

        Set<String> protectedTplIds = protectedMappings.stream()
                .map(PosPlaylistMappingDO::getTplUuid)
                .collect(Collectors.toSet());
        if (CollectionUtils.isNotEmpty(protectedTplIds)) {
            logger.info("protecting tpl for pos,tpl_ids:<{}>", protectedTplIds);
        }

        oldVersions.forEach(x -> {
            if (CollectionUtils.isEmpty(protectedTplIds) || !protectedTplIds
                    .contains(x.getPlaylistUuid())) {
                needToDeleteTplList.add(x);
            }
        });

        if (CollectionUtils.isNotEmpty(needToDeleteTplList)) {
            int size = needToDeleteTplList.size();
            List<Long> ids = new ArrayList<>(size);
            List<String> tplUUIDs = new ArrayList<>(size);
            needToDeleteTplList.stream().sorted(Comparator.comparingLong(TmsPlaylistDO::getId))
                    .forEach(x -> {
                        ids.add(x.getId());
                        tplUUIDs.add(x.getPlaylistUuid());
                    });
            // delete tpl
            tmsPlaylistService.removeByIds(ids);
            // send playlist-deletion.data
            TPlaylistDeletionDTO deletionDTO = MessageQueueConvert.MAPPER
                    .toTPlaylistDeletionDTO(dto.getOrganizationId(), null, dto.getPplUuid(),
                            tplUUIDs);
            mqService.playlistDeletionData(deletionDTO);
        }
    }

    private Map<String, TmsPlaylistDO> getUniqueIdMap(List<TmsPlaylistDO> doList) {
        if (CollectionUtils.isNotEmpty(doList)) {
            Map<String, TmsPlaylistDO> result = new HashMap<>(
                    (int) ((float) doList.size() / 0.75F + 1.0F));
            doList.forEach(x -> {
                PlaylistSourceEnum sourceEnum = PlaylistSourceEnum.getByCode(x.getSource());
                List<String> ids = new ArrayList<>();
                if (PlaylistSourceEnum.PRODUCER.equals(sourceEnum)) {
                    ids.add(x.getSourcePplId());
                    ids.add(x.getSourcePplVersionId());
                    if (StringUtils.isNotEmpty(x.getSourceSegmentSplitUuid())) {
                        List<SegmentSplit> segmentSplits = JSON
                                .parseObject(x.getSourceSegmentSplitUuid(),
                                        new TypeReference<List<SegmentSplit>>() {
                                        });
                        segmentSplits.forEach(y -> ids.add(y.getSegmentSplitUuid()));
                    }
                } else {
                    ids.add(x.getPlaylistUuid());
                    Optional.ofNullable(x.getSourceComplexId()).ifPresent(ids::add);
                }
                String uniqueId = UUID.nameUUIDFromBytes(
                        JSON.toJSONString(ids).getBytes(StandardCharsets.UTF_8)).toString();
                result.put(uniqueId, x);
            });
            return result;
        }
        return null;
    }

    private Map<String, String> genPosTplMap(List<String> posUuidList) {
        if (CollectionUtils.isNotEmpty(posUuidList)) {
            return new HashMap<>(
                    (int) ((float) posUuidList.size() / 0.75F + 1.0F));
        }
        return new HashMap<>();
    }
}
