package de.tonypsilon.bmm.backend.security;

import de.tonypsilon.bmm.backend.datatypes.IdAndLabel;
import de.tonypsilon.bmm.backend.match.service.MatchService;
import de.tonypsilon.bmm.backend.organization.service.OrganizationAdminService;
import de.tonypsilon.bmm.backend.season.service.SeasonService;
import de.tonypsilon.bmm.backend.season.service.SeasonStage;
import de.tonypsilon.bmm.backend.security.rnr.Roles;
import de.tonypsilon.bmm.backend.security.rnr.service.ClubAdminService;
import de.tonypsilon.bmm.backend.security.rnr.service.SeasonAdminService;
import de.tonypsilon.bmm.backend.security.rnr.service.TeamAdminService;
import de.tonypsilon.bmm.backend.team.data.TeamData;
import de.tonypsilon.bmm.backend.team.service.TeamService;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RestController;

import java.security.Principal;
import java.util.Comparator;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

@RestController
public class LoginController {

    private final SeasonAdminService seasonAdminService;
    private final ClubAdminService clubAdminService;
    private final TeamAdminService teamAdminService;
    private final TeamService teamService;
    private final OrganizationAdminService organizationAdminService;
    private final SeasonService seasonService;
    private final MatchService matchService;

    public LoginController(final SeasonAdminService seasonAdminService,
                           final ClubAdminService clubAdminService,
                           final TeamAdminService teamAdminService,
                           final TeamService teamService,
                           final OrganizationAdminService organizationAdminService,
                           final SeasonService seasonService,
                           final MatchService matchService) {
        this.seasonAdminService = seasonAdminService;
        this.clubAdminService = clubAdminService;
        this.teamAdminService = teamAdminService;
        this.teamService = teamService;
        this.organizationAdminService = organizationAdminService;
        this.seasonService = seasonService;
        this.matchService = matchService;
    }

    /**
     * Returns all information relevant to display an administration home page for the user.
     * @param user the given user.
     * @return an authentication response containing a boolean whether the user is admin, a list
     * of the seasons he is admin for, a list of clubs he is admin for, a list of organizations
     * he is admin for and where the season they belong to is neither completed nor archived and
     * finally a list of teams he is admin for and where the season they belong to is neither
     * completed nor archived.
     * All lists are in no particular order and all data is given in terms of IdAndLabel, the label
     * for displaying a human-readable name to the user and the id for further processing in
     * a different request.
     */
    @GetMapping(value = "/user", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<AuthenticationResponse> user(Principal user) {
        List<String> authorities = SecurityContextHolder.getContext()
                .getAuthentication().getAuthorities()
                .stream().map(GrantedAuthority::getAuthority).toList();
        return ResponseEntity.ok(
                new AuthenticationResponse(
                        // username
                        user.getName(),

                        // isAdmin
                        authorities.contains(Roles.ADMIN),

                        // seasons, if user is season admin
                        authorities.contains(Roles.SEASON_ADMIN) ?
                                seasonAdminService.getSeasonsOfSeasonAdmin(user.getName()).stream()
                                .map(seasonData -> new IdAndLabel(seasonData.id(), seasonData.name()))
                                .toList()
                                : List.of(),

                        // clubs, if user is club admin
                        authorities.contains(Roles.CLUB_ADMIN) ?
                                clubAdminService.getClubsOfClubAdmin(user.getName()).stream()
                                .map(clubData -> new IdAndLabel(clubData.id(), clubData.name()))
                                .toList()
                                : List.of(),

                        // organizations, if user is club admin
                        authorities.contains(Roles.CLUB_ADMIN) ?
                                organizationAdminService.getOrganizationsOfUser(user.getName()).stream()
                                .filter(organizationData -> Set.of(SeasonStage.REGISTRATION, SeasonStage.PREPARATION, SeasonStage.RUNNING)
                                        .contains(seasonService.getStageOfSeason(organizationData.seasonId())))
                                .map(organizationData -> new IdAndLabel(organizationData.id(), organizationData.name() + " - "
                                        + seasonService.getSeasonById(organizationData.seasonId()).name()))
                                .toList()
                                : List.of(),

                        // teams, if user is team admin
                        authorities.contains(Roles.TEAM_ADMIN) ?
                                teamAdminService.getTeamsOfTeamAdmin(user.getName()).stream()
                                .filter(teamData -> Set.of(SeasonStage.REGISTRATION, SeasonStage.PREPARATION, SeasonStage.RUNNING)
                                        .contains(seasonService.getStageOfSeason(teamService.getSeasonIdByTeamId(teamData.id()))))
                                .map(teamData -> new IdAndLabel(teamData.id(), teamService.getNameOfTeam(teamData.id())))
                                .toList()
                                : List.of(),

                        // matches, if user is team admin
                        authorities.contains(Roles.TEAM_ADMIN) ?
                                teamAdminService.getTeamsOfTeamAdmin(user.getName()).stream()
                                        .map(TeamData::id)
                                        .map(matchService::getMatchInfosOfOpenMatchesForTeam)
                                        .flatMap(List::stream)
                                        .sorted(Comparator.comparing(IdAndLabel::label))
                                        .distinct()
                                        .toList()
                                : List.of()
                )
        );
    }

    @PostMapping(value = "/administration/logout")
    public void logout() {
        // Actual logout handled by Spring framework.
    }
}
