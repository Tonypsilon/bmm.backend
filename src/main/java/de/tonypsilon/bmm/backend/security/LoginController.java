package de.tonypsilon.bmm.backend.security;

import de.tonypsilon.bmm.backend.datatypes.IdAndLabel;
import de.tonypsilon.bmm.backend.matchadministration.service.MatchAdministrationService;
import de.tonypsilon.bmm.backend.organization.service.OrganizationAdminService;
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

@RestController
public class LoginController {

    private final SeasonAdminService seasonAdminService;
    private final ClubAdminService clubAdminService;
    private final TeamAdminService teamAdminService;
    private final TeamService teamService;
    private final OrganizationAdminService organizationAdminService;

    public LoginController(final SeasonAdminService seasonAdminService,
                           final ClubAdminService clubAdminService,
                           final TeamAdminService teamAdminService,
                           final TeamService teamService,
                           final OrganizationAdminService organizationAdminService) {
        this.seasonAdminService = seasonAdminService;
        this.clubAdminService = clubAdminService;
        this.teamAdminService = teamAdminService;
        this.teamService = teamService;
        this.organizationAdminService = organizationAdminService;
    }

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
                                .map(organizationData -> new IdAndLabel(organizationData.id(), organizationData.name()))
                                .toList(),
                        teamAdminService.getTeamsOfTeamAdmin(user.getName()).stream()
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
