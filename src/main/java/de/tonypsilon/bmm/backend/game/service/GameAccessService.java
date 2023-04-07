package de.tonypsilon.bmm.backend.game.service;

import de.tonypsilon.bmm.backend.game.data.GameCreationData;
import de.tonypsilon.bmm.backend.security.rnr.service.AuthorizationService;
import org.springframework.stereotype.Service;

@Service
public class GameAccessService {

    private final GameService gameService;
    private final AuthorizationService authorizationService;

    public GameAccessService(final GameService gameService,
                             final AuthorizationService authorizationService) {
        this.gameService = gameService;
        this.authorizationService = authorizationService;
    }

    public void verifyGameCanBeCreated(String username, GameCreationData creationData) {
        // Todo: either the user is season admin of the season, or the match is OPEN and the user is
        // club admin / team admin related to the match of the game.
    }

    public void verifyResultCanBeChanged(String username, Long gameId) {
        // Todo: either the user is season admin of the season, or the match is OPEN and the user is
        // club admin / team admin related to the match of the game.
    }
}
