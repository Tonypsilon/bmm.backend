package de.tonypsilon.bmm.backend.season;

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

    @GetMapping(value = "/season/allNonArchived", produces = MediaType.APPLICATION_JSON_VALUE)
    public SeasonNamesResponse getAllNonArchivedSeasonNames() {
        return new SeasonNamesResponse(seasonService.getAllNonArchivedSeasonNames());
    }

    @RolesAllowed(Roles.ADMIN)
    @PostMapping(value = "/administration/season/create",
            consumes = MediaType.APPLICATION_JSON_VALUE,
            produces = MediaType.APPLICATION_JSON_VALUE)
    public SeasonName createSeason(@RequestBody SeasonName seasonName) {
        return new SeasonName(seasonService.createSeason(seasonName.seasonName()));
    }

}
