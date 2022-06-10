package de.tonypsilon.bmm.backend.season.facade;

import de.tonypsilon.bmm.backend.season.data.SeasonCreationData;
import de.tonypsilon.bmm.backend.season.data.SeasonData;
import de.tonypsilon.bmm.backend.season.service.SeasonService;
import de.tonypsilon.bmm.backend.security.Roles;
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

    public SeasonController(SeasonService seasonService) {
        this.seasonService = seasonService;
    }

    @GetMapping(value = "/seasons/non-archived", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<Collection<SeasonData>> getAllNonArchivedSeasons() {
        return ResponseEntity
                .ok(seasonService.getAllNonArchivedSeasons());
    }

    @RolesAllowed(Roles.ADMIN)
    @GetMapping(value = "/seasons/archived", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<Collection<SeasonData>> getAllArchivedSeasons() {
        return ResponseEntity
                .ok(seasonService.getAllArchivedSeasons());
    }

    @GetMapping(value = "/seasons/non-archived/{seasonName}", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<SeasonData> getNonArchivedSeason(@PathVariable String seasonName) {
        return ResponseEntity
                .ok(seasonService.getNonArchivedSeasonByName(seasonName));
    }

    @RolesAllowed(Roles.ADMIN)
    @GetMapping(value = "/seasons/archived/{seasonName}", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<SeasonData> getArchivedSeason(@PathVariable String seasonName) {
        return ResponseEntity
                .ok(seasonService.getArchivedSeasonByName(seasonName));
    }

    @RolesAllowed(Roles.ADMIN)
    @PostMapping(value = "/seasons",
            consumes = MediaType.APPLICATION_JSON_VALUE,
            produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<SeasonData> createSeason(RequestEntity<SeasonCreationData> seasonCreationData) {
        return ResponseEntity
                .status(HttpStatus.CREATED)
                .body(seasonService.createSeason(seasonCreationData.getBody().seasonName()));
    }

}
