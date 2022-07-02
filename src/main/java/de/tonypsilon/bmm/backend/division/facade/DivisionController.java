package de.tonypsilon.bmm.backend.division.facade;

import com.google.common.collect.SortedSetMultimap;
import de.tonypsilon.bmm.backend.division.data.DivisionData;
import de.tonypsilon.bmm.backend.division.service.DivisionService;
import de.tonypsilon.bmm.backend.season.service.SeasonService;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RestController;

@RestController
public class DivisionController {

    private final DivisionService divisionService;
    private final SeasonService seasonService;

    public DivisionController(DivisionService divisionService,
                              SeasonService seasonService) {
        this.divisionService = divisionService;
        this.seasonService = seasonService;
    }

    @GetMapping(value = "/divisions/non-archived/{seasonName}", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<SortedSetMultimap<Integer, DivisionData>> getAllDivisionsOfNonArchivedSeasonByLevel(
            @PathVariable String seasonName) {
        Long seasonId = seasonService.getNonArchivedSeasonByName(seasonName).id();
        return ResponseEntity.ok(
                divisionService.getAllDivisionsOfSeasonByLevel(seasonId)
        );
    }

    @GetMapping(value = "/divisions/archived/{seasonName}", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<SortedSetMultimap<Integer, DivisionData>> getAllDivisionsOfArchivedSeasonByLevel(
            @PathVariable String seasonName) {
        Long seasonId = seasonService.getArchivedSeasonByName(seasonName).id();
        return ResponseEntity.ok(
                divisionService.getAllDivisionsOfSeasonByLevel(seasonId)
        );
    }

}
