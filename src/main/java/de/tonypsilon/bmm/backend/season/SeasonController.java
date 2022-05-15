package de.tonypsilon.bmm.backend.season;

import de.tonypsilon.bmm.backend.security.Roles;
import jakarta.annotation.security.RolesAllowed;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

@RestController
public class SeasonController {

    private final SeasonService seasonService;

    public SeasonController(SeasonService seasonService) {
        this.seasonService = seasonService;
    }

    @GetMapping(value = "/season/allNonArchived")
    public List<String> getAllNonArchivedSeasonNames() {
        return seasonService.getAllNonArchivedSeasonNames();
    }

    @GetMapping(value = "/administration/season/allNonArchived")
    public List<String> getAllNonArchivedSeasonNamesProtected() {
        return seasonService.getAllNonArchivedSeasonNames();
    }

    //@RolesAllowed(Roles.ADMIN)
    @PostMapping(value = "/administration/season/create")
    public String createSeason(String seasonName) {
        return seasonService.createSeason(seasonName);
    }

}
