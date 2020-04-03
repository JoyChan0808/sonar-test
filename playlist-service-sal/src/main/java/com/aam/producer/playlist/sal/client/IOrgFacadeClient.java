package com.aam.producer.playlist.sal.client;


import com.aam.producer.playlist.protocol.request.OrganizationDTO;
import java.util.List;

public interface IOrgFacadeClient {

    List<String> getComplexGroupUuid(String orgId);

    List<OrganizationDTO> getOrganizations();

    List<String> getComplexUuid(List<String> complexGroups);

    Integer getDayOfWeek(String orgId);
}
