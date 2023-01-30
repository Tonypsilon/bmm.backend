package de.tonypsilon.bmm.backend.team.facade;

import de.tonypsilon.bmm.backend.division.service.DivisionService;
import de.tonypsilon.bmm.backend.security.rnr.Roles;
import de.tonypsilon.bmm.backend.security.rnr.service.AuthorizationService;
import de.tonypsilon.bmm.backend.team.data.TeamCreationData;
import de.tonypsilon.bmm.backend.team.data.TeamData;
import de.tonypsilon.bmm.backend.team.data.TeamDivisionAssignmentData;
import de.tonypsilon.bmm.backend.team.service.TeamService;
import org.springframework.http.*;
import org.springframework.lang.NonNull;
import org.springframework.web.bind.annotation.*;

import javax.annotation.security.RolesAllowed;
import java.security.Principal;
import java.util.Objects;

@RestController
public class TeamController {

    private final TeamService teamService;
    private final DivisionService divisionService;
    private final AuthorizationService authorizationService;

    public TeamController(final TeamService teamService,
                          final DivisionService divisionService,
                          final AuthorizationService authorizationService) {
        this.teamService = teamService;
        this.divisionService = divisionService;
        this.authorizationService = authorizationService;
    }

    @RolesAllowed(Roles.CLUB_ADMIN)
    @PostMapping(value = "/teams",
            consumes = MediaType.APPLICATION_JSON_VALUE,
            produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<TeamData> createTeam(
            RequestEntity<TeamCreationData> teamCreationDataRequestEntity,
            Principal principal) {
        TeamCreationData teamCreationData = Objects.requireNonNull(teamCreationDataRequestEntity.getBody());
        authorizationService.verifyUserIsClubAdminOfOrganization(principal.getName(), teamCreationData.organizationId());
        return ResponseEntity
                .status(HttpStatus.CREATED)
                .body(teamService.createTeam(teamCreationData));
    }

    @RolesAllowed(Roles.SEASON_ADMIN)
    @PatchMapping(value = "/teams/{id}")
    public ResponseEntity<TeamData> assignTeamToDivision(
            RequestEntity<TeamDivisionAssignmentData> teamDivisionAssignmentDataRequestEntity,
            @NonNull @PathVariable Long id,
            Principal principal) {
        TeamDivisionAssignmentData teamDivisionAssignmentData =
                Objects.requireNonNull(teamDivisionAssignmentDataRequestEntity.getBody());
        authorizationService.verifyUserIsSeasonAdminOfSeason(principal.getName(),
                divisionService.getSeasonIdByDivisionId(Objects.requireNonNull(teamDivisionAssignmentData.divisionId())));
        return ResponseEntity
                .ok(teamService.assignTeamToDivision(teamDivisionAssignmentData));
    }
}
