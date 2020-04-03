package com.aam.producer.playlist.biz.service.domain;

import com.aam.producer.playlist.repository.entity.TmsPlaylistDO;
import java.util.List;
import java.util.Set;

/**
 * playlist warning service
 *
 * @author oliver.lo
 * @since 2019-08-05 11:36
 */
public interface ITPlaylistWarningService {

    void verifyTpl(String pplUuid, String pplTitle, Set<String> tplUuidList);

    void verifyTyl(String pplUuid, String pplTitle, List<TmsPlaylistDO> tplList);
}
