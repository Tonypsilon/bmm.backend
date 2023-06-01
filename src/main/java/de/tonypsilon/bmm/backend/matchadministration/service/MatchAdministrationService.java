package de.tonypsilon.bmm.backend.matchadministration.service;

import de.tonypsilon.bmm.backend.match.data.MatchData;
import de.tonypsilon.bmm.backend.match.service.MatchService;
import de.tonypsilon.bmm.backend.matchadministration.data.MatchAdministrationData;
import de.tonypsilon.bmm.backend.organization.service.OrganizationService;
import de.tonypsilon.bmm.backend.security.rnr.service.ClubAdminService;
import de.tonypsilon.bmm.backend.security.rnr.service.TeamAdminService;
import de.tonypsilon.bmm.backend.team.data.TeamData;
import de.tonypsilon.bmm.backend.team.service.TeamService;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.function.Predicate;

@Service
public class MatchAdministrationService {

    private final ClubAdminService clubAdminService;
    private final TeamAdminService teamAdminService;
    private final OrganizationService organizationService;
    private final TeamService teamService;
    private final MatchService matchService;

    public MatchAdministrationService(final ClubAdminService clubAdminService,
                                      final TeamAdminService teamAdminService,
                                      final OrganizationService organizationService,
                                      final TeamService teamService,
                                      final MatchService matchService) {
        this.clubAdminService = clubAdminService;
        this.teamAdminService = teamAdminService;
        this.organizationService = organizationService;
        this.teamService = teamService;
        this.matchService = matchService;
    }


    public List<MatchAdministrationData> getMatchAdministrationDataForUser(String username) {
        return getMatchesForUser(username).stream()
                .map(MatchData::id)
                .map(MatchAdministrationData::new)
                .toList();
    }

    private List<MatchData> getMatchesForUser(String username) {
        return matchService.getAllOpenMatchesOfRunningSeasons().stream()
                .filter(isUserTeamAdminOfMatch(username)
                        .or(isUserClubAdminOfMatch(username)))
                .toList();
    }

    private Predicate<MatchData> isUserTeamAdminOfMatch(String username) {
        return matchData -> teamAdminService.isTeamAdmin(matchData.homeTeamId(), username)
                || teamAdminService.isTeamAdmin(matchData.awayTeamId(), username);
    }

    private Predicate<MatchData> isUserClubAdminOfMatch(String username) {
        return matchData -> isUserClubAdminOfTeam(username, teamService.getTeamDataById(matchData.homeTeamId()))
                || isUserClubAdminOfTeam(username, teamService.getTeamDataById(matchData.awayTeamId()));
    }

    private boolean isUserClubAdminOfTeam(String username, TeamData teamData) {
        return organizationService.getOrganizationById(teamData.organizationId())
                .clubIds().stream()
                .anyMatch(clubId -> clubAdminService.isClubAdmin(clubId, username));
    }
}
