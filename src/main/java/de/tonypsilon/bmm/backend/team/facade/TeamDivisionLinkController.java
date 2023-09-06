package de.tonypsilon.bmm.backend.team.facade;

import java.security.Principal;
import java.util.Objects;

import javax.annotation.security.RolesAllowed;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.RequestEntity;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RestController;

import de.tonypsilon.bmm.backend.security.rnr.Roles;
import de.tonypsilon.bmm.backend.security.rnr.service.AuthorizationService;
import de.tonypsilon.bmm.backend.team.data.TeamDivisionLinkData;
import de.tonypsilon.bmm.backend.team.service.TeamDivisionLinkService;
import de.tonypsilon.bmm.backend.team.service.TeamService;

@RestController
public class TeamDivisionLinkController {

	private final Logger logger = LoggerFactory.getLogger(TeamDivisionLinkController.class);
	private final TeamDivisionLinkService teamDivisionLinkService;
	private final AuthorizationService authorizationService;
	private final TeamService teamService;
	
	public TeamDivisionLinkController(
			final TeamDivisionLinkService teamDivisionLinkService,
			final AuthorizationService authorizationService,
			final TeamService teamService) {
		this.teamDivisionLinkService = teamDivisionLinkService;
		this.authorizationService = authorizationService;
		this.teamService = teamService;
	}
	
	@RolesAllowed(Roles.SEASON_ADMIN)
	@PostMapping(value = "/teamdivisionlinks",
	        consumes = MediaType.APPLICATION_JSON_VALUE,
	        produces = MediaType.APPLICATION_JSON_VALUE)
	public ResponseEntity<TeamDivisionLinkData> createTeamDivisionLink(
			RequestEntity<TeamDivisionLinkData> teamDivisionLinkDataRequestEntity,
			Principal principal) {
		TeamDivisionLinkData teamDivisionLinkData =
				Objects.requireNonNull(teamDivisionLinkDataRequestEntity).getBody();
		Objects.requireNonNull(teamDivisionLinkData);
		logger.info("User %s, POST on /teamdivisionlinks, body: %s"
				.formatted(principal.getName(), teamDivisionLinkData));
		authorizationService.verifyUserIsSeasonAdminOfSeason(principal.getName(),
				teamService.getSeasonIdByTeamId(Objects.requireNonNull(teamDivisionLinkData.teamId())));
		return ResponseEntity
				.status(HttpStatus.CREATED)
				.body(teamDivisionLinkService.createTeamDivisionLink(teamDivisionLinkData));
	}
	

}
