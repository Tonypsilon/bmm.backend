package de.tonypsilon.bmm.backend.security.rnr.facade;

import de.tonypsilon.bmm.backend.datatypes.IdAndLabel;
import de.tonypsilon.bmm.backend.exception.SeasonStageException;
import de.tonypsilon.bmm.backend.organization.service.OrganizationService;
import de.tonypsilon.bmm.backend.season.service.SeasonService;
import de.tonypsilon.bmm.backend.season.service.SeasonStage;
import de.tonypsilon.bmm.backend.security.rnr.Roles;
import de.tonypsilon.bmm.backend.security.rnr.data.TeamAdminData;
import de.tonypsilon.bmm.backend.security.rnr.service.AuthorizationService;
import de.tonypsilon.bmm.backend.security.rnr.service.TeamAdminService;
import de.tonypsilon.bmm.backend.team.data.TeamData;
import de.tonypsilon.bmm.backend.team.service.TeamService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.*;
import org.springframework.web.bind.annotation.*;

import javax.annotation.security.RolesAllowed;
import java.security.Principal;
import java.util.List;
import java.util.Objects;
import java.util.Set;

@RestController
public class TeamAdminController {

    private final Logger logger = LoggerFactory.getLogger(TeamAdminController.class);
    private final TeamAdminService teamAdminService;
    private final TeamService teamService;
    private final AuthorizationService authorizationService;
    private final OrganizationService organizationService;
    private final SeasonService seasonService;

    public TeamAdminController(final TeamAdminService teamAdminService,
                               final TeamService teamService,
                               final AuthorizationService authorizationService,
                               final OrganizationService organizationService,
                               final SeasonService seasonService) {
        this.teamAdminService = teamAdminService;
        this.teamService = teamService;
        this.authorizationService = authorizationService;
        this.organizationService = organizationService;
        this.seasonService = seasonService;
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

    @RolesAllowed(Roles.CLUB_ADMIN)
    @GetMapping(value = "/organization/{organizationId}/teams", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<List<IdAndLabel>> getAvailableTeams(Principal principal,
                                                              @PathVariable Long organizationId) {
        authorizationService.verifyUserIsClubAdminOfOrganization(
                principal.getName(), Objects.requireNonNull(organizationId));
        if(!Set.of(SeasonStage.REGISTRATION, SeasonStage.PREPARATION, SeasonStage.RUNNING).contains(
                seasonService.getStageOfSeason(organizationService.getSeasonIdOfOrganization(organizationId)))) {
            throw new SeasonStageException("In dieser Saisonphase können keine Teamadmins betrachtet werden!");
        }
        return ResponseEntity
                .ok(teamService.getTeamsOfOrganization(organizationId).stream()
                        .map(teamData -> new IdAndLabel(teamData.id(), teamData.name()))
                        .toList());
    }

}
