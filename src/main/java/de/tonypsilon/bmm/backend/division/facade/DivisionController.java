package de.tonypsilon.bmm.backend.division.facade;

import com.google.common.collect.SortedSetMultimap;
import de.tonypsilon.bmm.backend.division.data.DivisionCreationData;
import de.tonypsilon.bmm.backend.division.data.DivisionData;
import de.tonypsilon.bmm.backend.division.data.DivisionResultsData;
import de.tonypsilon.bmm.backend.division.service.DivisionResultsAssemblyService;
import de.tonypsilon.bmm.backend.division.service.DivisionService;
import de.tonypsilon.bmm.backend.season.service.SeasonService;
import de.tonypsilon.bmm.backend.security.rnr.Roles;
import javax.annotation.security.RolesAllowed;

import de.tonypsilon.bmm.backend.security.rnr.service.AuthorizationService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.RequestEntity;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RestController;

import java.security.Principal;
import java.util.List;
import java.util.Objects;

@RestController
public class DivisionController {

    private final Logger logger = LoggerFactory.getLogger(DivisionController.class);
    private final DivisionService divisionService;
    private final SeasonService seasonService;
    private final AuthorizationService authorizationService;
    private final DivisionResultsAssemblyService divisionResultsAssemblyService;

    public DivisionController(final DivisionService divisionService,
                              final SeasonService seasonService,
                              final AuthorizationService authorizationService,
                              final DivisionResultsAssemblyService divisionResultsAssemblyService) {
        this.divisionService = divisionService;
        this.seasonService = seasonService;
        this.authorizationService = authorizationService;
        this.divisionResultsAssemblyService = divisionResultsAssemblyService;
    }

    @GetMapping(value = "/divisions/{seasonName}", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<List<DivisionData>> getAllDivisionsOfNonArchivedSeasonByLevel(
            @PathVariable String seasonName) {
        Long seasonId = seasonService.getSeasonByName(seasonName).id();
        return ResponseEntity.ok(
                divisionService.getAllDivisionsOfSeason(seasonId)
        );
    }

    @RolesAllowed({Roles.SEASON_ADMIN})
    @PostMapping(value = "/divisions",
            consumes = MediaType.APPLICATION_JSON_VALUE,
            produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<DivisionData> createDivision(RequestEntity<DivisionCreationData> divisionCreationDataRequestEntity,
                                                       Principal principal) {
        DivisionCreationData divisionCreationData = Objects.requireNonNull(divisionCreationDataRequestEntity.getBody());
        Objects.requireNonNull(divisionCreationData);
        logger.info("User %s, POST on /divisions, body: %s".formatted(principal.getName(), divisionCreationData));
        authorizationService.verifyUserIsSeasonAdminOfSeason(principal.getName(), Objects.requireNonNull(divisionCreationData.seasonId()));
        return ResponseEntity
                .status(HttpStatus.CREATED)
                .body(divisionService.createDivision(divisionCreationData));
    }

    @GetMapping(value = "/divisions/{divisionId}/results", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<DivisionResultsData> getResultsForDivision(@PathVariable Long divisionId) {
        return ResponseEntity
                .ok(divisionResultsAssemblyService.assembleDivisionResults(Objects.requireNonNull(divisionId)));
    }

}
