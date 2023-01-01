package de.tonypsilon.bmm.backend.season.facade;

import de.tonypsilon.bmm.backend.exception.BadDataException;
import de.tonypsilon.bmm.backend.exception.SecurityException;
import de.tonypsilon.bmm.backend.season.data.CreateSeasonData;
import de.tonypsilon.bmm.backend.season.data.SeasonData;
import de.tonypsilon.bmm.backend.season.data.SeasonStageChangeData;
import de.tonypsilon.bmm.backend.season.service.SeasonService;
import de.tonypsilon.bmm.backend.security.rnr.Roles;
import de.tonypsilon.bmm.backend.security.rnr.service.SeasonAdminService;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.RequestEntity;
import org.springframework.http.ResponseEntity;
import org.springframework.lang.NonNull;
import org.springframework.web.bind.annotation.*;

import javax.annotation.security.RolesAllowed;
import java.security.Principal;
import java.util.Collection;

@RestController
public class SeasonController {

    private final SeasonService seasonService;
    private final SeasonAdminService seasonAdminService;

    public SeasonController(final SeasonService seasonService,
                            final SeasonAdminService seasonAdminService) {
        this.seasonService = seasonService;
        this.seasonAdminService = seasonAdminService;
    }

    @GetMapping(value = "/seasons", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<Collection<SeasonData>> getAllSeasons() {
        return ResponseEntity
                .ok(seasonService.getAllSeasons());
    }

    @GetMapping(value = "/seasons/{seasonName}", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<SeasonData> getSeasonByName(@NonNull @PathVariable String seasonName) {
        return ResponseEntity
                .ok(seasonService.getSeasonByName(seasonName));
    }

    @RolesAllowed(Roles.ADMIN)
    @PostMapping(value = "/seasons",
            consumes = MediaType.APPLICATION_JSON_VALUE,
            produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<SeasonData> createSeason(RequestEntity<CreateSeasonData> createSeasonDataRequestEntity) {
        CreateSeasonData createSeasonData = createSeasonDataRequestEntity.getBody();
        if (createSeasonData == null) {
            throw new BadDataException("Unvollständige Daten gegeben!");
        }
        return ResponseEntity
                .status(HttpStatus.CREATED)
                .body(seasonService.createSeason(createSeasonData));
    }

    @RolesAllowed(Roles.SEASON_ADMIN)
    @PatchMapping(value = "/seasons/{seasonName}",
            produces = MediaType.APPLICATION_JSON_VALUE,
            consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<SeasonData> changeSeasonState(RequestEntity<SeasonStageChangeData> patchedSeasonRequestEntity,
                                                        @NonNull @PathVariable String seasonName,
                                                        Principal principal) {
        SeasonStageChangeData seasonStageChangeData = patchedSeasonRequestEntity.getBody();
        if(seasonStageChangeData == null) {
            throw new BadDataException("Unvollständige Daten gegeben!");
        }
        if (!seasonName.equals(seasonStageChangeData.seasonName())) {
            throw new BadDataException("Der Saisonname im Request passt nicht zum Requestbody.");
        }
        if (!seasonAdminService.isSeasonAdmin(seasonName, principal.getName())) {
            throw new SecurityException("Der Benutzer %s ist kein Admin für die Saison %s"
                    .formatted(principal.getName(), seasonName));
        }
        return ResponseEntity
                .ok(seasonService.updateSeasonStage(patchedSeasonRequestEntity.getBody()));
    }


}
