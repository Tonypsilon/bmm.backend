package de.tonypsilon.bmm.backend.security.rnr.facade;

import de.tonypsilon.bmm.backend.security.rnr.Roles;
import de.tonypsilon.bmm.backend.security.rnr.data.SeasonAdminData;
import de.tonypsilon.bmm.backend.security.rnr.service.SeasonAdminService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.*;
import org.springframework.web.bind.annotation.*;

import javax.annotation.security.RolesAllowed;
import java.security.Principal;
import java.util.Collection;
import java.util.Objects;


@RestController
public class SeasonAdminController {

    private final Logger logger = LoggerFactory.getLogger(SeasonAdminController.class);
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
    public ResponseEntity<SeasonAdminData> createSeasonAdmin(
            RequestEntity<SeasonAdminData> seasonAdminDataRequestEntity,
            Principal principal) {
        SeasonAdminData seasonAdminData = Objects.requireNonNull(seasonAdminDataRequestEntity).getBody();
        Objects.requireNonNull(seasonAdminData);
        logger.info("User %s, POST on /seasonadmins, body: %s".formatted(principal.getName(), seasonAdminData));
        return ResponseEntity
                .status(HttpStatus.CREATED)
                .body(seasonAdminService.createSeasonAdmin(seasonAdminData));
    }

    @RolesAllowed(Roles.ADMIN)
    @DeleteMapping(value = "/seasonadmins", consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<Void> deleteSeasonAdmin(RequestEntity<SeasonAdminData> seasonAdminDataRequestEntity,
                                                  Principal principal) {
        SeasonAdminData deleteSeasonAdminData = Objects.requireNonNull(seasonAdminDataRequestEntity).getBody();
        Objects.requireNonNull(deleteSeasonAdminData);
        logger.info("User %s, DELETE on /seasonadmins, body: %s".formatted(principal.getName(), deleteSeasonAdminData));
        seasonAdminService.deleteSeasonAdmin(deleteSeasonAdminData);
        return ResponseEntity.noContent().build();
    }
}
