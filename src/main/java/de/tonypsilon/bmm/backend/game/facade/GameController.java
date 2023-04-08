package de.tonypsilon.bmm.backend.game.facade;

import de.tonypsilon.bmm.backend.game.data.GameCreationData;
import de.tonypsilon.bmm.backend.game.data.GameData;
import de.tonypsilon.bmm.backend.game.service.GameAccessService;
import de.tonypsilon.bmm.backend.game.service.GameService;
import de.tonypsilon.bmm.backend.game.service.ResultData;
import de.tonypsilon.bmm.backend.security.rnr.Roles;
import org.springframework.http.*;
import org.springframework.web.bind.annotation.*;

import javax.annotation.security.RolesAllowed;
import java.security.Principal;
import java.util.Objects;

@RestController
public class GameController {

    private final GameAccessService gameAccessService;
    private final GameService gameService;

    public GameController(final GameAccessService gameAccessService,
                          final GameService gameService) {
        this.gameAccessService = gameAccessService;
        this.gameService = gameService;
    }

    @RolesAllowed({Roles.SEASON_ADMIN, Roles.CLUB_ADMIN, Roles.TEAM_ADMIN})
    @PostMapping(value = "/games",
            consumes = MediaType.APPLICATION_JSON_VALUE,
            produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<GameData> createGame(RequestEntity<GameCreationData> creationDataRequestEntity,
                                               Principal principal) {
        GameCreationData creationData = Objects.requireNonNull(creationDataRequestEntity).getBody();
        Objects.requireNonNull(creationData);
        gameAccessService.verifyGameCanBeCreated(principal.getName(), creationData);
        return ResponseEntity
                .status(HttpStatus.CREATED)
                        .body(gameService.createGame(creationData));
    }

    @RolesAllowed({Roles.SEASON_ADMIN, Roles.CLUB_ADMIN, Roles.TEAM_ADMIN})
    @PatchMapping(value = "/games/{gameId}/playedResult",
            consumes = MediaType.APPLICATION_JSON_VALUE,
            produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<GameData> changePlayedResult(RequestEntity<ResultData> resultDataRequestEntity,
                                                       @PathVariable Long gameId,
                                                       Principal principal) {
        ResultData resultData = Objects.requireNonNull(resultDataRequestEntity).getBody();
        Objects.requireNonNull(resultData);
        Objects.requireNonNull(resultData.homeResult());
        Objects.requireNonNull(resultData.awayResult());
        gameAccessService.verifyResultCanBeChanged(principal.getName(), gameId);
        return ResponseEntity
                .ok(gameService.changeResult(gameId, resultData));
    }

    @RolesAllowed({Roles.SEASON_ADMIN, Roles.CLUB_ADMIN, Roles.TEAM_ADMIN})
    @PatchMapping(value = "/games/{gameId}/overruledResult",
            consumes = MediaType.APPLICATION_JSON_VALUE,
            produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<GameData> changeOverruledResult(RequestEntity<ResultData> resultDataRequestEntity,
                                                       @PathVariable Long gameId,
                                                       Principal principal) {
        ResultData resultData = Objects.requireNonNull(resultDataRequestEntity).getBody();
        Objects.requireNonNull(resultData);
        Objects.requireNonNull(resultData.homeResult());
        Objects.requireNonNull(resultData.awayResult());
        gameAccessService.verifyResultCanBeChanged(principal.getName(), gameId);
        return ResponseEntity
                .ok(gameService.changeOverruledResult(gameId, resultData));
    }
}
