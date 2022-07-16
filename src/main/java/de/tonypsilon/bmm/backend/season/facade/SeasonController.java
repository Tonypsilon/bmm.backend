package de.tonypsilon.bmm.backend.season.facade;

import de.tonypsilon.bmm.backend.season.data.SeasonCreationData;
import de.tonypsilon.bmm.backend.season.data.SeasonData;
import de.tonypsilon.bmm.backend.season.data.SeasonStageChangeData;
import de.tonypsilon.bmm.backend.season.service.SeasonService;
import de.tonypsilon.bmm.backend.security.rnr.Roles;
import jakarta.annotation.security.RolesAllowed;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.RequestEntity;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Collection;

@RestController
public class SeasonController {

    private final SeasonService seasonService;

    public SeasonController(final SeasonService seasonService) {
        this.seasonService = seasonService;
    }

    @GetMapping(value = "/seasons", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<Collection<SeasonData>> getAllSeasons() {
        return ResponseEntity
                .ok(seasonService.getAllSeasons());
    }

    @GetMapping(value = "/seasons/{seasonName}", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<SeasonData> getSeasonByName(@PathVariable String seasonName) {
        return ResponseEntity
                .ok(seasonService.getSeasonByName(seasonName));
    }

    @RolesAllowed(Roles.ADMIN)
    @PostMapping(value = "/seasons",
            consumes = MediaType.APPLICATION_JSON_VALUE,
            produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<SeasonData> createSeason(RequestEntity<SeasonCreationData> seasonCreationDataRequestEntity) {
        return ResponseEntity
                .status(HttpStatus.CREATED)
                .body(seasonService.createSeason(seasonCreationDataRequestEntity.getBody()));
    }

    @RolesAllowed({Roles.ADMIN, Roles.SEASON_ADMIN})
    @PatchMapping(value = "/seasons/{seasonName}",
            produces = MediaType.APPLICATION_JSON_VALUE,
            consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<SeasonData> changeSeasonState(RequestEntity<SeasonStageChangeData> patchedSeasonRequestEntity) {
        return ResponseEntity
                .ok(seasonService.updateSeasonStage(patchedSeasonRequestEntity.getBody()));
    }


}
