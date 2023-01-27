package de.tonypsilon.bmm.backend.team.facade;

import de.tonypsilon.bmm.backend.division.service.DivisionService;
import de.tonypsilon.bmm.backend.organization.service.OrganizationService;
import de.tonypsilon.bmm.backend.security.rnr.Roles;
import de.tonypsilon.bmm.backend.security.rnr.service.SeasonAdminService;
import de.tonypsilon.bmm.backend.team.data.TeamCreationData;
import de.tonypsilon.bmm.backend.team.data.TeamData;
import de.tonypsilon.bmm.backend.team.data.TeamDivisionAssignmentData;
import de.tonypsilon.bmm.backend.team.service.TeamService;
import org.springframework.http.*;
import org.springframework.lang.NonNull;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.web.bind.annotation.*;

import javax.annotation.security.RolesAllowed;
import java.security.Principal;
import java.util.Objects;

@RestController
public class TeamController {

    private final TeamService teamService;
    private final OrganizationService organizationService;
    private final SeasonAdminService seasonAdminService;
    private final DivisionService divisionService;

    public TeamController(final TeamService teamService,
                          final OrganizationService organizationService,
                          final SeasonAdminService seasonAdminService,
                          final DivisionService divisionService) {
        this.teamService = teamService;
        this.organizationService = organizationService;
        this.seasonAdminService = seasonAdminService;
        this.divisionService = divisionService;
    }

    @RolesAllowed(Roles.CLUB_ADMIN)
    @PostMapping(value = "/teams",
            consumes = MediaType.APPLICATION_JSON_VALUE,
            produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<TeamData> createTeam(
            RequestEntity<TeamCreationData> teamCreationDataRequestEntity,
            Principal principal) {
        TeamCreationData teamCreationData = Objects.requireNonNull(teamCreationDataRequestEntity.getBody());
        if(!organizationService.isClubAdminOfOrganization(principal.getName(), teamCreationData.organizationId())){
            throw new AccessDeniedException("Der Benutzer hat nicht genügend Vereinsadministrationsrechte!");
        }
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
        if(!seasonAdminService.isSeasonAdmin(divisionService.getSeasonIdByDivisionId(
                Objects.requireNonNull(teamDivisionAssignmentData.divisionId())), principal.getName())) {
            throw new AccessDeniedException("Der Benutzer hat nicht die benötigten Saisonadministrationsrechte!");
        }
        return ResponseEntity
                .ok(teamService.assignTeamToDivision(teamDivisionAssignmentData));
    }
}
