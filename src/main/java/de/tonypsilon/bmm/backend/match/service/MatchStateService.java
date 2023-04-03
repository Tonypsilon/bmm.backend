package de.tonypsilon.bmm.backend.match.service;

import de.tonypsilon.bmm.backend.match.data.MatchData;
import de.tonypsilon.bmm.backend.match.data.MatchState;
import de.tonypsilon.bmm.backend.security.rnr.service.AuthorizationService;
import org.springframework.stereotype.Service;

@Service
public class MatchStateService {

    private final MatchService matchService;

    private final AuthorizationService authorizationService;

    public MatchStateService(final MatchService matchService,
                             final AuthorizationService authorizationService) {
        this.matchService = matchService;
        this.authorizationService = authorizationService;
    }

    public MatchData changeMatchState(MatchData matchData, MatchState state, String username) {
        // Todo check if user is allowed to change

        return matchService.changeMatchState(matchData.id(), state);
    }

}
