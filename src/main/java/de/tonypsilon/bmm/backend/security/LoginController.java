package de.tonypsilon.bmm.backend.security;

import de.tonypsilon.bmm.backend.datatypes.IdAndLabel;
import de.tonypsilon.bmm.backend.matchadministration.service.MatchAdministrationService;
import de.tonypsilon.bmm.backend.organization.service.OrganizationAdminService;
import de.tonypsilon.bmm.backend.season.service.SeasonService;
import de.tonypsilon.bmm.backend.season.service.SeasonStage;
import de.tonypsilon.bmm.backend.security.rnr.Roles;
import de.tonypsilon.bmm.backend.security.rnr.service.ClubAdminService;
import de.tonypsilon.bmm.backend.security.rnr.service.SeasonAdminService;
import de.tonypsilon.bmm.backend.security.rnr.service.TeamAdminService;
import de.tonypsilon.bmm.backend.team.service.TeamService;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RestController;

import java.security.Principal;
import java.util.Set;

@RestController
public class LoginController {

    private final SeasonAdminService seasonAdminService;
    private final ClubAdminService clubAdminService;
    private final TeamAdminService teamAdminService;
    private final TeamService teamService;
    private final OrganizationAdminService organizationAdminService;
    private final SeasonService seasonService;

    public LoginController(final SeasonAdminService seasonAdminService,
                           final ClubAdminService clubAdminService,
                           final TeamAdminService teamAdminService,
                           final TeamService teamService,
                           final OrganizationAdminService organizationAdminService,
                           final SeasonService seasonService) {
        this.seasonAdminService = seasonAdminService;
        this.clubAdminService = clubAdminService;
        this.teamAdminService = teamAdminService;
        this.teamService = teamService;
        this.organizationAdminService = organizationAdminService;
        this.seasonService = seasonService;
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
        return ResponseEntity.ok(
                new AuthenticationResponse(user.getName(),
                        SecurityContextHolder.getContext().getAuthentication().getAuthorities()
                                .stream().map(GrantedAuthority::getAuthority).toList()
                                .contains(Roles.ADMIN),
                        seasonAdminService.getSeasonsOfSeasonAdmin(user.getName()).stream()
                                .map(seasonData -> new IdAndLabel(seasonData.id(), seasonData.name()))
                                .toList(),
                        clubAdminService.getClubsOfClubAdmin(user.getName()).stream()
                                .map(clubData -> new IdAndLabel(clubData.id(), clubData.name()))
                                .toList(),
                        organizationAdminService.getOrganizationsOfUser(user.getName()).stream()
                                .filter(organizationData -> Set.of(SeasonStage.REGISTRATION, SeasonStage.PREPARATION, SeasonStage.RUNNING)
                                        .contains(seasonService.getStageOfSeason(organizationData.seasonId())))
                                .map(organizationData -> new IdAndLabel(organizationData.id(), organizationData.name()))
                                .toList(),
                        teamAdminService.getTeamsOfTeamAdmin(user.getName()).stream()
                                .filter(teamData -> Set.of(SeasonStage.REGISTRATION, SeasonStage.PREPARATION, SeasonStage.RUNNING)
                                        .contains(seasonService.getStageOfSeason(teamService.getSeasonIdByTeamId(teamData.id()))))
                                .map(teamData -> new IdAndLabel(teamData.id(), teamService.getNameOfTeam(teamData.id())))
                                .toList()
                )
        );
    }

    @PostMapping(value = "/administration/logout")
    public void logout() {
        // Actual logout handled by Spring framework.
    }
}
