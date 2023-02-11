package de.tonypsilon.bmm.backend.season.facade;

import de.tonypsilon.bmm.backend.season.data.PlayingDateCreationData;
import de.tonypsilon.bmm.backend.season.data.PlayingDateData;
import de.tonypsilon.bmm.backend.season.service.PlayingDateService;
import de.tonypsilon.bmm.backend.security.rnr.Roles;
import de.tonypsilon.bmm.backend.security.rnr.service.AuthorizationService;
import org.springframework.http.*;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.annotation.security.RolesAllowed;
import java.security.Principal;
import java.util.Objects;

@RestController
public class PlayingDateController {

    private final PlayingDateService playingDateService;
    private final AuthorizationService authorizationService;

    public PlayingDateController(final PlayingDateService playingDateService,
                                 final AuthorizationService authorizationService) {
        this.playingDateService = playingDateService;
        this.authorizationService = authorizationService;
    }

    @RolesAllowed(Roles.SEASON_ADMIN)
    @PostMapping(value = "/playingdates",
            consumes = MediaType.APPLICATION_JSON_VALUE,
            produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<PlayingDateData> createPlayingDate(
            RequestEntity<PlayingDateCreationData> playingDateCreationDataRequestEntity,
            Principal principal) {
        PlayingDateCreationData creationData = Objects.requireNonNull(playingDateCreationDataRequestEntity).getBody();
        Objects.requireNonNull(creationData);
        authorizationService.verifyUserIsSeasonAdminOfSeason(principal.getName(),
                Objects.requireNonNull(creationData.seasonId()));
        return ResponseEntity
                .status(HttpStatus.CREATED)
                .body(playingDateService.createPlayingDate(creationData));
    }
}
