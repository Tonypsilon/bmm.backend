package de.tonypsilon.bmm.backend.season.facade;

import de.tonypsilon.bmm.backend.season.data.SeasonData;
import de.tonypsilon.bmm.backend.season.service.SeasonService;
import de.tonypsilon.bmm.backend.security.Roles;
import jakarta.annotation.security.RolesAllowed;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.*;

@RestController
public class SeasonController {

    private final SeasonService seasonService;

    public SeasonController(SeasonService seasonService) {
        this.seasonService = seasonService;
    }

    @GetMapping(value = "/seasons/names/non-archived", produces = MediaType.APPLICATION_JSON_VALUE)
    public SeasonNamesResponse getAllNonArchivedSeasonNames() {
        return new SeasonNamesResponse(seasonService.getAllNonArchivedSeasonNames());
    }

    // For now this only exists for testing, might be removed later on.
    @RolesAllowed(Roles.ADMIN)
    @GetMapping(value = "/seasons/{seasonName}",
    produces = MediaType.APPLICATION_JSON_VALUE)
    public SeasonApiData findSeason(@PathVariable String seasonName) {
        return seasonDataToSeasonResponse(seasonService.getSeasonByName(seasonName));
    }

    @RolesAllowed(Roles.ADMIN)
    @PostMapping(value = "/seasons",
            produces = MediaType.APPLICATION_JSON_VALUE)
    public SeasonApiData createSeason(@RequestParam SeasonName seasonName) {
        return seasonDataToSeasonResponse(seasonService.createSeason(seasonName.seasonName()));
    }

    @RolesAllowed(Roles.ADMIN)
    @PutMapping(value = "/seasons/{seasonName}")
    public SeasonApiData changeSeason(SeasonApiData seasonApiData) {
        // Todo real implementation
        return seasonApiData;
    }

    private SeasonApiData seasonDataToSeasonResponse(SeasonData seasonData) {
        return new SeasonApiData(seasonData.name(), seasonData.stage());
    }

}
