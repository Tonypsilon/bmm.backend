package de.tonypsilon.bmm.backend.team.facade;

import java.security.Principal;
import java.util.Objects;

import javax.annotation.security.RolesAllowed;

import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.RequestEntity;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RestController;

import de.tonypsilon.bmm.backend.security.rnr.Roles;
import de.tonypsilon.bmm.backend.security.rnr.service.AuthorizationService;
import de.tonypsilon.bmm.backend.team.data.TeamDivisionAssignmentData;
import de.tonypsilon.bmm.backend.team.service.TeamDivisionAssignmentService;
import de.tonypsilon.bmm.backend.team.service.TeamService;

@RestController
public class TeamDivisionAssignmentController {
	
	private final TeamDivisionAssignmentService teamDivisionAssignmentService;
	private final AuthorizationService authorizationService;
	private final TeamService teamService;
	
	public TeamDivisionAssignmentController(
			final TeamDivisionAssignmentService teamDivisionAssignmentService,
			final AuthorizationService authorizationService,
			final TeamService teamService) {
		this.teamDivisionAssignmentService = teamDivisionAssignmentService;
		this.authorizationService = authorizationService;
		this.teamService = teamService;
	}
	
	@RolesAllowed(Roles.SEASON_ADMIN)
	@PostMapping(value = "/teamdivisionassignments", 
	        consumes = MediaType.APPLICATION_JSON_VALUE,
	        produces = MediaType.APPLICATION_JSON_VALUE)
	public ResponseEntity<TeamDivisionAssignmentData> createTeamDivisionAssignment(
			RequestEntity<TeamDivisionAssignmentData> teamDivisionAssignmentDataRequestEntity,
			Principal principal) {
		TeamDivisionAssignmentData teamDivisionAssignmentData =
				Objects.requireNonNull(teamDivisionAssignmentDataRequestEntity).getBody();
		Objects.requireNonNull(teamDivisionAssignmentData);
		authorizationService.verifyUserIsSeasonAdminOfSeason(principal.getName(),
				teamService.getSeasonIdByTeamId(Objects.requireNonNull(teamDivisionAssignmentData.teamId())));
		return ResponseEntity
				.status(HttpStatus.CREATED)
				.body(teamDivisionAssignmentService.createTeamDivisionAssignment(teamDivisionAssignmentData));
	}
	

}
