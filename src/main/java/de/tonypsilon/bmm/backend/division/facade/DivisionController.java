package de.tonypsilon.bmm.backend.division.facade;

import com.google.common.collect.SortedSetMultimap;
import de.tonypsilon.bmm.backend.division.data.DivisionCreationData;
import de.tonypsilon.bmm.backend.division.data.DivisionData;
import de.tonypsilon.bmm.backend.division.service.DivisionService;
import de.tonypsilon.bmm.backend.season.service.SeasonService;
import de.tonypsilon.bmm.backend.security.rnr.Roles;
import javax.annotation.security.RolesAllowed;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.RequestEntity;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.Objects;

@RestController
public class DivisionController {

    private final DivisionService divisionService;
    private final SeasonService seasonService;

    public DivisionController(DivisionService divisionService,
                              SeasonService seasonService) {
        this.divisionService = divisionService;
        this.seasonService = seasonService;
    }

    @GetMapping(value = "/divisions/{seasonName}", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<SortedSetMultimap<Integer, DivisionData>> getAllDivisionsOfNonArchivedSeasonByLevel(
            @PathVariable String seasonName) {
        Long seasonId = seasonService.getSeasonByName(seasonName).id();
        return ResponseEntity.ok(
                divisionService.getAllDivisionsOfSeasonByLevel(seasonId)
        );
    }

    @RolesAllowed({Roles.SEASON_ADMIN})
    @PostMapping(value = "/divisions",
            consumes = MediaType.APPLICATION_JSON_VALUE,
            produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<DivisionData> createDivision(RequestEntity<DivisionCreationData> divisionCreationDataRequestEntity) {
        return ResponseEntity
                .status(HttpStatus.CREATED)
                .body(divisionService.createDivision(Objects.requireNonNull(divisionCreationDataRequestEntity.getBody())));
    }

}
