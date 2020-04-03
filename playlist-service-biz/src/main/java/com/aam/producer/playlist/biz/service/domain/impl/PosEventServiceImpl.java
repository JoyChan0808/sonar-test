package com.aam.producer.playlist.biz.service.domain.impl;

import com.aam.producer.playlist.biz.convert.MessageQueueConvert;
import com.aam.producer.playlist.biz.service.domain.IMessageQueueService;
import com.aam.producer.playlist.biz.service.domain.IPosEventService;
import com.aam.producer.playlist.biz.service.domain.IPosMappingService;
import com.aam.producer.playlist.common.utils.TimeUtils;
import com.aam.producer.playlist.protocol.request.PosDataDTO;
import com.aam.producer.playlist.protocol.request.TitleDataDTO;
import com.aam.utils.exception.BizException;
import com.alibaba.fastjson.JSON;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;
import org.apache.commons.collections4.CollectionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class PosEventServiceImpl implements IPosEventService {

    private static final Logger logger = LoggerFactory.getLogger(PosEventServiceImpl.class);

    private final ConcurrentHashMap<String, Integer> titleCplMap = new ConcurrentHashMap<>();

    private final IMessageQueueService mqService;
    private final IPosMappingService posMappingService;

    @Autowired
    public PosEventServiceImpl(
            IMessageQueueService mqService,
            IPosMappingService posMappingService) {
        this.mqService = mqService;
        this.posMappingService = posMappingService;
    }

    @Override
    public void posDataHandler(PosDataDTO dto) {
        logger.info(
                "receive pos.data,complex_uuid:<{}>,pos_uuid:<{}>,tpl_uuid:<{}>,title_uuid:<{}>,last_modified:<{}>,state:<{}>,message:<{}>",
                dto.getComplexUuid(), dto.getPosUuid(), dto.getTplUuid(), dto.getTitleUuid(),
                dto.getPosLastModified(), dto.getState(), dto.getMessage());
        if (logger.isDebugEnabled()) {
            logger.debug("receive pos.data,pos:<{}>", dto.toString());
        }

        Long start = TimeUtils.changeMillisecondTimestamp(dto.getStart());
        if (start == null || start <= System.currentTimeMillis()) {
            // start is empty or expired
            return;
        }

        try {
            boolean needMore = posMappingService.handlePosData(dto);
            if (needMore) {
                // send pos.fetch
                mqService.posFetch(MessageQueueConvert.MAPPER
                        .toPosFetchDTO(dto.getComplexUuid(), dto.getWeekNumber(),
                                Collections.singletonList(dto.getPosUuid())));
            } else {
                // send pos-match.data
                mqService.posMatchData(dto);
            }
        } catch (BizException e) {
            logger.warn(
                    "pos.data handle failed,complex_uuid:<{}>,pos_uuid:<{}>,message:<{}>",
                    dto.getComplexUuid(), dto.getPosUuid(), e.getMessage());
        }
    }

    @Override
    public void titleDataHandler(TitleDataDTO dto) {
        logger.info("receive message title.data,dto:<{}>", JSON.toJSONString(dto));
       /* String titleId = getTitleThatCplChanged(dto);
        if (StringUtils.isEmpty(titleId)) {
            logger.info("receive message title.data,don't need to handle.");
            return;
        }
        logger.info("receive message title.data,rebuild segment split for it,title_id:<{}>",
                titleId);*/

        posMappingService.handleTitleData(dto);
    }

    private String getTitleThatCplChanged(TitleDataDTO dto) {
        String titleId = dto.getTitleUuid();
        if (Boolean.TRUE.equals(dto.getDeleted())) {
            titleCplMap.remove(titleId);
            return null;
        }
        List<String> cplIds = dto.getCplIds();
        int compare = CollectionUtils.isEmpty(cplIds) ? 1 : cplIds.hashCode();
        if (titleCplMap.containsKey(titleId)) {
            if (Objects.equals(compare, titleCplMap.get(titleId))) {
                return null;
            }
        } else {
            titleCplMap.put(titleId, compare);
        }
        return titleId;
    }
}
