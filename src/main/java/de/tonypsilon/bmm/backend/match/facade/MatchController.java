package de.tonypsilon.bmm.backend.match.facade;

import de.tonypsilon.bmm.backend.exception.BadDataException;
import de.tonypsilon.bmm.backend.match.data.MatchData;
import de.tonypsilon.bmm.backend.match.data.MatchStateChangeData;
import de.tonypsilon.bmm.backend.match.data.RichMatchData;
import de.tonypsilon.bmm.backend.match.service.MatchService;
import de.tonypsilon.bmm.backend.match.service.MatchStateService;
import de.tonypsilon.bmm.backend.match.service.RichMatchInformationAssemblyService;
import de.tonypsilon.bmm.backend.security.rnr.Roles;
import de.tonypsilon.bmm.backend.security.rnr.service.AuthorizationService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.MediaType;
import org.springframework.http.RequestEntity;
import org.springframework.http.ResponseEntity;
import org.springframework.lang.NonNull;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PatchMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RestController;

import javax.annotation.security.RolesAllowed;
import java.security.Principal;
import java.util.Objects;
import java.util.Set;

@RestController
public class MatchController {

    // Match creation only happens from within the SeasonStartService.

    private final Logger logger = LoggerFactory.getLogger(MatchController.class);
    private final MatchStateService matchStateService;
    private final AuthorizationService authorizationService;
    private final MatchService matchService;
    private final RichMatchInformationAssemblyService richMatchInformationAssemblyService;

    public MatchController(final MatchStateService matchStateService,
                           final AuthorizationService authorizationService,
                           final MatchService matchService,
                           final RichMatchInformationAssemblyService richMatchInformationAssemblyService) {
        this.matchStateService = matchStateService;
        this.authorizationService = authorizationService;
        this.matchService = matchService;
        this.richMatchInformationAssemblyService = richMatchInformationAssemblyService;
    }

    @RolesAllowed({Roles.SEASON_ADMIN, Roles.CLUB_ADMIN, Roles.TEAM_ADMIN})
    @PatchMapping(value = "/matches/{matchId}",
            produces = MediaType.APPLICATION_JSON_VALUE,
            consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<MatchData> changeMatchState(RequestEntity<MatchStateChangeData> patchedMatchRequestEntity,
                                                      @NonNull @PathVariable Long matchId,
                                                      Principal principal) {
        MatchStateChangeData changeData = Objects.requireNonNull(patchedMatchRequestEntity).getBody();
        Objects.requireNonNull(changeData);
        logger.info("User %s, PATCH on /matches/%s, body: %s".formatted(principal.getName(), matchId, changeData));
        if(!matchId.equals(Objects.requireNonNull(changeData.matchId()))) {
            throw new BadDataException("Die Match ID im Request passt nicht zum Requestbody!");
        }
        return ResponseEntity
                .ok(matchStateService.changeMatchState(matchId, changeData.state(), principal.getName()));
    }

    @RolesAllowed(Roles.TEAM_ADMIN)
    @GetMapping(value = "/matches/{matchId}/info")
    public ResponseEntity<RichMatchData> getRichMatchInformation(Principal principal,
                                                                 @PathVariable Long matchId) {
        Objects.requireNonNull(matchId);
        MatchData matchData = matchService.getMatchDataById(matchId);
        authorizationService.verifyUserIsTeamAdminOfAnyTeam(principal.getName(),
                Set.of(matchData.homeTeamId(), matchData.awayTeamId()));
        return ResponseEntity
                .ok(richMatchInformationAssemblyService.assembleRichMatchData(matchId));
    }

}
