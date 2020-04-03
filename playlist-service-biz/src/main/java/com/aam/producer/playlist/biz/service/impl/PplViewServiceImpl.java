package com.aam.producer.playlist.biz.service.impl;

import com.aam.producer.playlist.biz.service.IPplViewService;
import com.aam.producer.playlist.repository.dao.PplViewMapper;
import com.aam.producer.playlist.repository.entity.PplViewDO;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;

@Service
public class PplViewServiceImpl extends ServiceImpl<PplViewMapper, PplViewDO> implements
        IPplViewService {

}
