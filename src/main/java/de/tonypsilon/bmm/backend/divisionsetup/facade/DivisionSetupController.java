package de.tonypsilon.bmm.backend.divisionsetup.facade;

import de.tonypsilon.bmm.backend.datatypes.IdAndLabel;
import de.tonypsilon.bmm.backend.division.data.DivisionData;
import de.tonypsilon.bmm.backend.division.service.DivisionService;
import de.tonypsilon.bmm.backend.divisionsetup.data.DivisionSetupFoundationData;
import de.tonypsilon.bmm.backend.divisionsetup.service.DivisionSetupService;
import de.tonypsilon.bmm.backend.security.rnr.Roles;
import de.tonypsilon.bmm.backend.security.rnr.service.AuthorizationService;
import de.tonypsilon.bmm.backend.team.data.TeamDivisionLinkData;
import de.tonypsilon.bmm.backend.team.service.TeamDivisionLinkService;
import de.tonypsilon.bmm.backend.team.service.TeamService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.MediaType;
import org.springframework.http.RequestEntity;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import javax.annotation.security.RolesAllowed;
import java.security.Principal;
import java.util.List;
import java.util.Objects;

@RestController
public class DivisionSetupController {

    private final Logger logger = LoggerFactory.getLogger(DivisionSetupController.class);
    private final AuthorizationService authorizationService;
    private final DivisionSetupService divisionSetupService;
    private final DivisionService divisionService;
    private final TeamDivisionLinkService teamDivisionLinkService;
    private final TeamService teamService;

    public DivisionSetupController(final AuthorizationService authorizationService,
                                   final DivisionSetupService divisionSetupService,
                                   final DivisionService divisionService,
                                   final TeamDivisionLinkService teamDivisionLinkService,
                                   final TeamService teamService) {
        this.authorizationService = authorizationService;
        this.divisionSetupService = divisionSetupService;
        this.divisionService = divisionService;
        this.teamDivisionLinkService = teamDivisionLinkService;
        this.teamService = teamService;
    }

    @RolesAllowed(Roles.SEASON_ADMIN)
    @GetMapping(value = "/seasons/{seasonId}/divisionsetup", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<DivisionSetupFoundationData> getDivisionSetup(Principal principal,
                                                                        @PathVariable Long seasonId) {
        authorizationService.verifyUserIsSeasonAdminOfSeason(principal.getName(), Objects.requireNonNull(seasonId));
        List<DivisionData> availableDivisions = divisionService.getAllDivisionsOfSeason(seasonId)
                .stream().toList();
        List<IdAndLabel> availableTeams = teamService.getTeamsBySeasonId(seasonId).stream()
                .map(teamData -> new IdAndLabel(teamData.id(), teamData.name()))
                .toList();
        List<TeamDivisionLinkData> currentLinks = teamDivisionLinkService.getBySeason(seasonId).stream().toList();
        return ResponseEntity
                .ok(new DivisionSetupFoundationData(availableDivisions, availableTeams, currentLinks));
    }

    @RolesAllowed(Roles.SEASON_ADMIN)
    @PutMapping(value = "/seasons/{seasonId}/divisionsetup",
            consumes = MediaType.APPLICATION_JSON_VALUE,
            produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<List<TeamDivisionLinkData>> putDivisionSetup(
            RequestEntity<List<TeamDivisionLinkData>> teamDivisionLinksRequestEntity,
            Principal principal,
            @PathVariable Long seasonId
    ) {
        authorizationService.verifyUserIsSeasonAdminOfSeason(principal.getName(), Objects.requireNonNull(seasonId));
        List<TeamDivisionLinkData> teamDivisionLinks =
                Objects.requireNonNull(teamDivisionLinksRequestEntity).getBody();
        Objects.requireNonNull(teamDivisionLinks);
        logger.info("User %s, PUT on /seasons/%d/divisionsetup, body: %s"
                .formatted(principal.getName(), seasonId, teamDivisionLinks));
        return ResponseEntity
                .ok(divisionSetupService.putTeamDivisionLinksForSeason(
                        seasonId,
                        teamDivisionLinks.stream()
                                .filter(tdl -> tdl.teamId() != null
                                        && tdl.divisionId() != null
                                        && tdl.number() != null)
                                .toList()));
    }
}
