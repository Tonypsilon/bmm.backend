package de.tonypsilon.bmm.backend.security.rnr.facade;

import de.tonypsilon.bmm.backend.security.rnr.Roles;
import de.tonypsilon.bmm.backend.security.rnr.data.TeamAdminData;
import de.tonypsilon.bmm.backend.security.rnr.service.AuthorizationService;
import de.tonypsilon.bmm.backend.security.rnr.service.TeamAdminService;
import de.tonypsilon.bmm.backend.team.data.TeamData;
import de.tonypsilon.bmm.backend.team.service.TeamService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.*;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.annotation.security.RolesAllowed;
import java.security.Principal;
import java.util.Objects;

@RestController
public class TeamAdminController {

    private final Logger logger = LoggerFactory.getLogger(TeamAdminController.class);
    private final TeamAdminService teamAdminService;
    private final TeamService teamService;
    private final AuthorizationService authorizationService;

    public TeamAdminController(final TeamAdminService teamAdminService,
                               final TeamService teamService,
                               final AuthorizationService authorizationService) {
        this.teamAdminService = teamAdminService;
        this.teamService = teamService;
        this.authorizationService = authorizationService;
    }

    @RolesAllowed(Roles.CLUB_ADMIN)
    @PostMapping(value = "/teamadmins",
            consumes = MediaType.APPLICATION_JSON_VALUE,
            produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<TeamAdminData> createTeamAdmin(RequestEntity<TeamAdminData> teamAdminDataRequestEntity,
                                                         Principal principal) {
        TeamAdminData teamAdminData = Objects.requireNonNull(teamAdminDataRequestEntity).getBody();
        Objects.requireNonNull(teamAdminData);
        logger.info("User %s, POST on /teamadmins, body: %s".formatted(principal.getName(), teamAdminData));
        Objects.requireNonNull(teamAdminData.username());
        Long teamId = Objects.requireNonNull(teamAdminData.teamId());
        TeamData teamData = teamService.getTeamDataById(teamId);
        authorizationService.verifyUserIsClubAdminOfOrganization(principal.getName(), teamData.organizationId());
        return ResponseEntity
                .status(HttpStatus.CREATED)
                .body(teamAdminService.createTeamAdmin(teamAdminData));
    }

    @RolesAllowed(Roles.CLUB_ADMIN)
    @DeleteMapping(value = "/teamadmins", consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<Void> deleteTeamAdmin(RequestEntity<TeamAdminData> teamAdminDataRequestEntity,
                                                Principal principal) {
        TeamAdminData teamAdminData = Objects.requireNonNull(teamAdminDataRequestEntity).getBody();
        Objects.requireNonNull(teamAdminData);
        logger.info("User %s, DELETE on /teamadmins, body: %s".formatted(principal.getName(), teamAdminData));
        Objects.requireNonNull(teamAdminData.username());
        Long teamId = Objects.requireNonNull(teamAdminData.teamId());
        TeamData teamData = teamService.getTeamDataById(teamId);
        authorizationService.verifyUserIsClubAdminOfOrganization(principal.getName(), teamData.organizationId());
        teamAdminService.deleteTeamAdmin(teamAdminData);
        return ResponseEntity.noContent().build();
    }

}
