package com.aam.producer.playlist.sal.client;


import com.aam.producer.playlist.protocol.response.SplitComplexInfo;
import com.aam.producer.playlist.sal.response.ComplexGroup;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * complex service
 *
 * @author oliver.lo
 * @since 2019/6/11 11:25 AM
 */
public interface IComplexFacadeClient {

    String getLmsUuid(String complexId);

    void lmsCacheEvict(String complexId);

    String getOrganizationUuid(String complexId);

    void orgCacheEvict(String complexId);

    String getComplexLocale(String complexUuid);

    List<SplitComplexInfo> getComplex(List<String> complexGroup);

    Map<String, String> getComplexNameMap(List<String> complexUuids);

    ComplexGroup getComplexGroup(String groupUuid);

    Map<String, String> getAddressCountry(Set<String> complexUuids);

    Map<String, List<String>> getComplexDeviceInfo(String complexUuid);

}
