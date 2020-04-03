package com.aam.producer.playlist.biz.service.impl;

import com.aam.producer.lib.enums.SegmentTypeEnum;
import com.aam.producer.lib.protocol.TaskDTO;
import com.aam.producer.playlist.biz.service.ISegmentEventService;
import com.aam.producer.playlist.biz.service.ISegmentService;
import com.aam.producer.playlist.biz.service.domain.ISegmentViewService;
import com.aam.producer.playlist.biz.util.OrgUtil;
import com.aam.producer.playlist.protocol.message.SegmentDTO;
import com.aam.producer.playlist.repository.entity.SegmentDO;
import com.aam.utils.utils.SpringContextUtils;
import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.serializer.SerializerFeature;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.kafka.core.KafkaTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
public class SegmentEventServiceImpl implements ISegmentEventService {

    private final KafkaTemplate<String, String> tranKafkaTemplate;
    private final ISegmentViewService iSegmentViewService;
    @Value("${kafka.topics.segmentData}")
    private String segmentDataRequest;

    @Autowired
    public SegmentEventServiceImpl(
            KafkaTemplate<String, String> tranKafkaTemplate,
            ISegmentViewService iSegmentViewService) {
        this.tranKafkaTemplate = tranKafkaTemplate;
        this.iSegmentViewService = iSegmentViewService;
    }


    @Override
    @Transactional
    public void sendDataEvent(String uuid, boolean deleted) {
        SegmentDO segmentDO;
        if (deleted) {
            segmentDO = new SegmentDO();
            segmentDO.setUuid(uuid);
            segmentDO.setDeleted(true);
        } else {
            ISegmentService iSegmentService = SpringContextUtils.getBean(ISegmentService.class);
            segmentDO = iSegmentService.getSegmentDO(uuid);
            segmentDO.setOrganizationId(OrgUtil.orgContenter.get());
            segmentDO.setDeleted(false);
        }
        SegmentDTO segmentMessage = new SegmentDTO();
        BeanUtils.copyProperties(segmentDO, segmentMessage);
        segmentMessage.setType(SegmentTypeEnum.getByCode(segmentDO.getType()).getName());
        TaskDTO<SegmentDTO> data = new TaskDTO<>(segmentDataRequest, segmentMessage);
        tranKafkaTemplate.send(segmentDataRequest,
                JSON.toJSONString(data, SerializerFeature.WriteMapNullValue));
    }

}
