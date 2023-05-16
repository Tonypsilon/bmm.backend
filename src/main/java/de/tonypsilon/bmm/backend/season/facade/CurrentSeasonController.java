package de.tonypsilon.bmm.backend.season.facade;

import de.tonypsilon.bmm.backend.season.data.SeasonData;
import de.tonypsilon.bmm.backend.season.service.CurrentSeasonService;
import de.tonypsilon.bmm.backend.security.rnr.Roles;
import org.springframework.http.*;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.annotation.security.RolesAllowed;
import java.util.Objects;

@RestController
public class CurrentSeasonController {

    private final CurrentSeasonService currentSeasonService;

    public CurrentSeasonController(final CurrentSeasonService currentSeasonService) {
        this.currentSeasonService = currentSeasonService;
    }

    @GetMapping(value = "/currentseason", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<SeasonData> getCurrentSeason() {
        return ResponseEntity
                .ok(currentSeasonService.getCurrentSeason());
    }

    @RolesAllowed(Roles.ADMIN)
    @PostMapping(value = "/currentseason",
            consumes = MediaType.APPLICATION_JSON_VALUE,
            produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<SeasonData> setCurrentSeason(RequestEntity<Long> currentSeasonIdRequestEntity) {
        Objects.requireNonNull(currentSeasonIdRequestEntity);
        return ResponseEntity
                .status(HttpStatus.CREATED)
                        .body(currentSeasonService.setCurrentSeason(
                                Objects.requireNonNull(currentSeasonIdRequestEntity.getBody())));
    }

}
