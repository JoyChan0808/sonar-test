package com.aam.producer.playlist.biz.service.domain;

import com.aam.producer.task.protocol.enums.TaskTypeEnum;

public interface ITaskReactorService {

    void sendSegmentSplitEvent(String pplUuid, String titleUuid,
            String contentAssociationUuid, TaskTypeEnum type, Boolean deleted,String segmentType);

}
