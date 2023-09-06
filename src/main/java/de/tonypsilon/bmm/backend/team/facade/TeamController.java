package de.tonypsilon.bmm.backend.team.facade;

import de.tonypsilon.bmm.backend.security.rnr.Roles;
import de.tonypsilon.bmm.backend.security.rnr.service.AuthorizationService;
import de.tonypsilon.bmm.backend.team.data.TeamCreationData;
import de.tonypsilon.bmm.backend.team.data.TeamData;
import de.tonypsilon.bmm.backend.team.service.TeamService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.*;
import org.springframework.web.bind.annotation.*;

import javax.annotation.security.RolesAllowed;
import java.security.Principal;
import java.util.Objects;

@RestController
public class TeamController {

    private final Logger logger = LoggerFactory.getLogger(TeamController.class);
    private final TeamService teamService;
    private final AuthorizationService authorizationService;

    public TeamController(final TeamService teamService,
                          final AuthorizationService authorizationService) {
        this.teamService = teamService;
        this.authorizationService = authorizationService;
    }

    @RolesAllowed(Roles.CLUB_ADMIN)
    @PostMapping(value = "/teams",
            consumes = MediaType.APPLICATION_JSON_VALUE,
            produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<TeamData> createTeam(
            RequestEntity<TeamCreationData> teamCreationDataRequestEntity,
            Principal principal) {
        TeamCreationData teamCreationData = Objects.requireNonNull(teamCreationDataRequestEntity).getBody();
        Objects.requireNonNull(teamCreationData);
        logger.info("User %s: POST on /teams, body: %s".formatted(principal.getName(), teamCreationData));
        authorizationService.verifyUserIsClubAdminOfOrganization(principal.getName(), teamCreationData.organizationId());
        return ResponseEntity
                .status(HttpStatus.CREATED)
                .body(teamService.createTeam(teamCreationData));
    }

}
