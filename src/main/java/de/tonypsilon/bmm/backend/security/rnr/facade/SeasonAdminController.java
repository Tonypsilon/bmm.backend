package de.tonypsilon.bmm.backend.security.rnr.facade;

import de.tonypsilon.bmm.backend.security.rnr.Roles;
import de.tonypsilon.bmm.backend.security.rnr.data.SeasonAdminData;
import de.tonypsilon.bmm.backend.security.rnr.service.SeasonAdminService;
import javax.annotation.security.RolesAllowed;
import org.springframework.http.*;
import org.springframework.web.bind.annotation.*;

import java.security.Principal;
import java.util.Collection;


@RestController
public class SeasonAdminController {

    private final SeasonAdminService seasonAdminService;

    public SeasonAdminController(final SeasonAdminService seasonAdminService) {
        this.seasonAdminService = seasonAdminService;
    }

    @RolesAllowed(Roles.ADMIN)
    @GetMapping(value = "/seasonadmins", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<Collection<SeasonAdminData>> getAllSeasonAdmins() {
        return ResponseEntity
                .ok(seasonAdminService.getAllSeasonAdmins());
    }

    @RolesAllowed(Roles.ADMIN)
    @PostMapping(value = "/seasonadmins",
            consumes = MediaType.APPLICATION_JSON_VALUE,
            produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<SeasonAdminData> createSeasonAdmin(RequestEntity<SeasonAdminData> seasonAdminDataRequestEntity,
                                                             Principal principal) {
        return ResponseEntity
                .status(HttpStatus.CREATED)
                .body(seasonAdminService.createSeasonAdmin(seasonAdminDataRequestEntity.getBody()));
    }

    @RolesAllowed(Roles.ADMIN)
    @DeleteMapping(value = "/seasonadmins")
    public ResponseEntity<Void> deleteSeasonAdmin(RequestEntity<SeasonAdminData> seasonAdminDataRequestEntity,
                                  Principal principal) {
        seasonAdminService.deleteSeasonAdmin(seasonAdminDataRequestEntity.getBody().seasonId(),
                seasonAdminDataRequestEntity.getBody().username());
        return ResponseEntity.noContent().build();
    }
}
