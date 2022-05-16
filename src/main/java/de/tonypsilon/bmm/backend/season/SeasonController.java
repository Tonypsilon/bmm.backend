package de.tonypsilon.bmm.backend.season;

import de.tonypsilon.bmm.backend.security.Roles;
import jakarta.annotation.security.RolesAllowed;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.*;

import java.util.List;

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

    @GetMapping(value = "/administration/season/allNonArchived")
    public List<String> getAllNonArchivedSeasonNamesProtected() {
        return seasonService.getAllNonArchivedSeasonNames();
    }

    @RolesAllowed(Roles.ADMIN)
    @PostMapping(value = "/administration/season/create")
    public String createSeason(@RequestBody String seasonName) {
        return seasonService.createSeason(seasonName);
    }

}
