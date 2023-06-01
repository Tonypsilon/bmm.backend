package de.tonypsilon.bmm.backend.game.service;

import de.tonypsilon.bmm.backend.game.data.GameCreationData;
import de.tonypsilon.bmm.backend.match.data.MatchData;
import de.tonypsilon.bmm.backend.match.data.MatchState;
import de.tonypsilon.bmm.backend.match.service.MatchService;
import de.tonypsilon.bmm.backend.matchday.service.MatchdayService;
import de.tonypsilon.bmm.backend.security.rnr.service.AuthorizationService;
import de.tonypsilon.bmm.backend.security.rnr.service.SeasonAdminService;
import org.springframework.stereotype.Service;

import java.util.Set;

@Service
public class GameAccessService {

    private final GameService gameService;
    private final MatchService matchService;
    private final MatchdayService matchdayService;
    private final AuthorizationService authorizationService;
    private final SeasonAdminService seasonAdminService;

    public GameAccessService(final GameService gameService,
                             final MatchService matchService,
                             final MatchdayService matchdayService,
                             final AuthorizationService authorizationService,
                             final SeasonAdminService seasonAdminService) {
        this.gameService = gameService;
        this.matchService = matchService;
        this.matchdayService = matchdayService;
        this.authorizationService = authorizationService;
        this.seasonAdminService = seasonAdminService;
    }

    public void verifyGameCanBeCreated(String username, GameCreationData creationData) {
        verifyUserIsAuthorizedToWorkOnMatch(username,
                matchService.getMatchDataById(creationData.matchId()));

    }

    public void verifyResultCanBeChanged(String username, Long gameId) {
        verifyUserIsAuthorizedToWorkOnMatch(username,
                matchService.getMatchDataById(gameService.getGameDataById(gameId).matchId()));
    }

    /**
     * Either the match is open and the user is clubadmin or teamadmin or the user is season admin,
     * then the state of the match does not matter.
     * @param username
     * @param matchData
     */
    private void verifyUserIsAuthorizedToWorkOnMatch(String username, MatchData matchData) {
        if(matchData.matchState() == MatchState.OPEN
                && !seasonAdminService.isSeasonAdmin(
                matchdayService.getSeasonIdOfMatchday(matchData.matchdayId()), username)) {
            authorizationService.verifyUserIsClubAdminOrTeamAdminOfAnyTeam(username,
                    Set.of(matchData.homeTeamId(), matchData.awayTeamId()));
        } else {
            authorizationService.verifyUserIsSeasonAdminOfSeason(username,
                    matchdayService.getSeasonIdOfMatchday(matchData.matchdayId()));
        }
    }
}
