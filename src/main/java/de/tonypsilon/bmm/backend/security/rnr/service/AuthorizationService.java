package de.tonypsilon.bmm.backend.security.rnr.service;

import de.tonypsilon.bmm.backend.organization.data.OrganizationData;
import de.tonypsilon.bmm.backend.organization.service.OrganizationService;
import de.tonypsilon.bmm.backend.security.rnr.data.TeamAdminData;
import de.tonypsilon.bmm.backend.team.data.TeamData;
import de.tonypsilon.bmm.backend.team.service.TeamService;
import org.springframework.lang.NonNull;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Collection;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Utility methods to verify if a given user has not only a role but is also associated to a certain target.
 * All methods throw an {@link AccessDeniedException} in case a condition is not fulfilled.
 */
@Service
public class AuthorizationService {

    private final ClubAdminService clubAdminService;
    private final OrganizationService organizationService;
    private final SeasonAdminService seasonAdminService;

    private final TeamAdminService teamAdminService;
    private final TeamService teamService;

    public AuthorizationService(final ClubAdminService clubAdminService,
                                final OrganizationService organizationService,
                                final SeasonAdminService seasonAdminService,
                                final TeamAdminService teamAdminService,
                                final TeamService teamService) {
        this.clubAdminService = clubAdminService;
        this.organizationService = organizationService;
        this.seasonAdminService = seasonAdminService;
        this.teamAdminService = teamAdminService;
        this.teamService = teamService;
    }

    /**
     * Throws an AccessDeniedException in case the given user is no club admin for any of the given clubs.
     * @param username the name of the user
     * @param clubIds the ids of the clubs
     */
    @Transactional
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
        verifyUserIsClubAdminOfAnyOrganization(username, Set.of(organizationId));
    }

    /**
     * Throws an AccessDeniedException in case the given user is no club admin for any of the given organizations.
     * @param username the name of the user
     * @param organizationIds the ids of the organization
     */
    @Transactional
    public void verifyUserIsClubAdminOfAnyOrganization(
            @NonNull String username, @NonNull Collection<Long> organizationIds) {
        verifyUserIsClubAdminOfAnyClub(username,
                organizationIds.stream()
                        .map(organizationService::getOrganizationById)
                        .map(OrganizationData::clubIds)
                        .flatMap(Set::stream)
                        .collect(Collectors.toSet()));
    }

    /**
     * Throws an AccessDeniedException in case the given user is no season admin for the given season.
     * @param username the name of the user
     * @param seasonId the id of the season
     */
    @Transactional
    public void verifyUserIsSeasonAdminOfSeason(@NonNull String username, @NonNull Long seasonId) {
        if(!seasonAdminService.isSeasonAdmin(seasonId, username)) {
            throw new AccessDeniedException("%s hat nicht ausreichend Saisonadministrationsrechte!"
                    .formatted(username));
        }
    }

    /**
     * Throws an AccessDeniedException in case the given user is no club admin for the given team.
     * @param username
     * @param teamId
     */
    @Transactional
    public void verifyUserIsClubAdminOfTeam(@NonNull String username, @NonNull Long teamId) {
        verifyUserIsClubAdminOfOrganization(username, teamService.getTeamDataById(teamId).organizationId());
    }

    /**
     * Throws an AccessDeniedException in case the given user is no club admin for any of the given teams.
     * @param username
     * @param teamIds
     */
    @Transactional
    public void verifyUserIsClubAdminOrTeamAdminOfAnyTeam(@NonNull String username, @NonNull Collection<Long> teamIds) {
        if(teamIds.stream()
                .map(teamAdminService::getTeamAdminsOfTeam)
                .flatMap(Set::stream)
                .map(TeamAdminData::username)
                .noneMatch(username::equals))
            verifyUserIsClubAdminOfAnyOrganization(username,
                teamIds.stream()
                        .map(teamService::getTeamDataById)
                        .map(TeamData::organizationId)
                        .collect(Collectors.toSet()));
    }

    @Transactional
    public void verifyUserIsTeamAdminOfAnyTeam(@NonNull String username, @NonNull Set<Long> teamIds) {
        if(teamAdminService.getTeamsOfTeamAdmin(username).stream()
                .map(TeamData::id)
                .noneMatch(teamIds::contains)) {
            throw new AccessDeniedException("%s hat nicht ausreichend Mannschaftsadministrationsrechte!");
        }
    }

}
