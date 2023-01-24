package de.tonypsilon.bmm.backend.team.facade;

import de.tonypsilon.bmm.backend.organization.service.OrganizationService;
import de.tonypsilon.bmm.backend.security.rnr.Roles;
import de.tonypsilon.bmm.backend.team.data.TeamCreationData;
import de.tonypsilon.bmm.backend.team.data.TeamData;
import de.tonypsilon.bmm.backend.team.service.TeamService;
import org.springframework.http.*;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.annotation.security.RolesAllowed;
import java.security.Principal;
import java.util.Objects;

@RestController
public class TeamController {

    private final TeamService teamService;
    private final OrganizationService organizationService;

    public TeamController(final TeamService teamService,
                          final OrganizationService organizationService) {
        this.teamService = teamService;
        this.organizationService = organizationService;
    }

    @RolesAllowed(Roles.CLUB_ADMIN)
    @PostMapping(value = "/teams",
            consumes = MediaType.APPLICATION_JSON_VALUE,
            produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<TeamData> createTeam(
            RequestEntity<TeamCreationData> teamCreationDataRequestEntity,
            Principal principal) {
        TeamCreationData teamCreationData = Objects.requireNonNull(teamCreationDataRequestEntity.getBody());
        organizationService.isClubAdminOfOrganization(principal.getName(), teamCreationData.organizationId());
        return ResponseEntity
                .status(HttpStatus.CREATED)
                .body(teamService.createTeam(teamCreationData));
    }
}
