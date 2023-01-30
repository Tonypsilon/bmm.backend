package de.tonypsilon.bmm.backend.security.rnr.service;

import de.tonypsilon.bmm.backend.organization.service.OrganizationService;
import org.springframework.lang.NonNull;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Set;

/**
 * Utility methods to verify if a given user has not only a role but is also associated to a certain target.
 * All methods throw an {@link AccessDeniedException} in case a condition is not fulfilled.
 */
@Service
public class AuthorizationService {

    private final ClubAdminService clubAdminService;
    private final OrganizationService organizationService;
    private final SeasonAdminService seasonAdminService;

    public AuthorizationService(final ClubAdminService clubAdminService,
                                final OrganizationService organizationService,
                                final SeasonAdminService seasonAdminService) {
        this.clubAdminService = clubAdminService;
        this.organizationService = organizationService;
        this.seasonAdminService = seasonAdminService;
    }

    /**
     * Throws an AccessDeniedException in case the given user is no club admin for any of the given clubs.
     * @param username the name of the user
     * @param clubIds the ids of the clubs
     */
    public void verifyUserIsClubAdminOfAnyClub(@NonNull String username, @NonNull Set<Long> clubIds) {
        if (clubIds.stream()
                .map(clubAdminService::getAdminsOfClub)
                .flatMap(Set::stream)
                .noneMatch(username::equals)) {
            throw new AccessDeniedException("%s hat nicht ausreichend Vereinsadministrationsrechte!"
                    .formatted(username));
        }
    }

    /**
     * Throws an AccessDeniedException in case the given user is no club admin for the given organization.
     * @param username the name of the user
     * @param organizationId the id of the organization
     */
    @Transactional
    public void verifyUserIsClubAdminOfOrganization(@NonNull String username, @NonNull Long organizationId) {
        verifyUserIsClubAdminOfAnyClub(username,
                organizationService.getOrganizationById(organizationId).clubIds());
    }

    /**
     * Throws an AccessDeniedException in case the given user is no season admin for the given season.
     * @param username the name of the user
     * @param seasonId the id of the season
     */
    public void verifyUserIsSeasonAdminOfSeason(@NonNull String username, @NonNull Long seasonId) {
        if(!seasonAdminService.isSeasonAdmin(seasonId, username)) {
            throw new AccessDeniedException("%s hat nicht ausreichend Saisonadministrationsrechte!"
                    .formatted(username));
        }
    }


}
