package de.tonypsilon.bmm.backend.security.rnr.facade;

import de.tonypsilon.bmm.backend.organization.service.OrganizationService;
import de.tonypsilon.bmm.backend.security.rnr.Roles;
import de.tonypsilon.bmm.backend.security.rnr.data.TeamAdminData;
import de.tonypsilon.bmm.backend.security.rnr.service.TeamAdminService;
import de.tonypsilon.bmm.backend.team.data.TeamData;
import de.tonypsilon.bmm.backend.team.service.TeamService;
import org.springframework.http.*;
import org.springframework.lang.NonNull;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.annotation.security.RolesAllowed;
import java.security.Principal;
import java.util.Objects;

@RestController
public class TeamAdminController {

    private final TeamAdminService teamAdminService;
    private final TeamService teamService;
    private final OrganizationService organizationService;

    public TeamAdminController(final TeamAdminService teamAdminService,
                               final TeamService teamService,
                               final OrganizationService organizationService) {
        this.teamAdminService = teamAdminService;
        this.teamService = teamService;
        this.organizationService = organizationService;
    }

    @RolesAllowed(Roles.CLUB_ADMIN)
    @PostMapping(value = "/teamadmins",
            consumes = MediaType.APPLICATION_JSON_VALUE,
            produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<TeamAdminData> createTeamAdmin(RequestEntity<TeamAdminData> teamAdminDataRequestEntity,
                                                         Principal principal) {
        TeamAdminData teamAdminData = Objects.requireNonNull(teamAdminDataRequestEntity.getBody());
        Objects.requireNonNull(teamAdminData.username());
        Long teamId = Objects.requireNonNull(teamAdminData.teamId());
        TeamData teamData = teamService.getTeamDataById(teamId);
        verifyUserIsClubAdminOfOrganization(principal.getName(), teamData.organizationId());
        return ResponseEntity
                .status(HttpStatus.CREATED)
                .body(teamAdminService.createTeamAdmin(teamAdminDataRequestEntity.getBody()));
    }

    @RolesAllowed(Roles.CLUB_ADMIN)
    @DeleteMapping(value = "/teamadmins", consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<Void> deleteTeamAdmin(RequestEntity<TeamAdminData> teamAdminDataRequestEntity,
                                                Principal principal) {
        TeamAdminData teamAdminData = Objects.requireNonNull(teamAdminDataRequestEntity.getBody());
        Objects.requireNonNull(teamAdminData.username());
        Long teamId = Objects.requireNonNull(teamAdminData.teamId());
        TeamData teamData = teamService.getTeamDataById(teamId);
        verifyUserIsClubAdminOfOrganization(principal.getName(), teamData.organizationId());
        teamAdminService.deleteTeamAdmin(teamAdminData);
        return ResponseEntity.noContent().build();
    }

    private void verifyUserIsClubAdminOfOrganization(@NonNull String username, @NonNull Long organizationId) {
        if (!organizationService.isClubAdminOfOrganization(username, organizationId)) {
            throw new AccessDeniedException("Benutzer %s ist kein Admin für Organisation mit ID %d!"
                    .formatted(username, organizationId));
        }
    }
}
