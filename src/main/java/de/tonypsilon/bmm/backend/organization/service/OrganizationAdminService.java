package de.tonypsilon.bmm.backend.organization.service;

import de.tonypsilon.bmm.backend.club.data.ClubData;
import de.tonypsilon.bmm.backend.organization.data.OrganizationData;
import de.tonypsilon.bmm.backend.security.rnr.service.ClubAdminService;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

@Service
public class OrganizationAdminService {

    private final ClubAdminService clubAdminService;

    private final OrganizationService organizationService;

    public OrganizationAdminService(final ClubAdminService clubAdminService,
                                    final OrganizationService organizationService) {
        this.clubAdminService = clubAdminService;
        this.organizationService = organizationService;
    }

    @NonNull
    public Set<OrganizationData> getOrganizationsOfUser(String username) {
        List<Long> clubIdsOfUser = clubAdminService.getClubsOfClubAdmin(username).stream()
                .map(ClubData::id)
                .toList();
        return organizationService.getAll().stream()
                .filter(organizationData -> organizationData.clubIds().stream().anyMatch(clubIdsOfUser::contains))
                .collect(Collectors.toSet());
    }
}
