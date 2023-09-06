package de.tonypsilon.bmm.backend.security.rnr.facade;

import de.tonypsilon.bmm.backend.security.rnr.Roles;
import de.tonypsilon.bmm.backend.security.rnr.data.ClubAdminData;
import de.tonypsilon.bmm.backend.security.rnr.service.ClubAdminService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.*;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.annotation.security.RolesAllowed;
import java.security.Principal;
import java.util.Objects;

@RestController
public class ClubAdminController {

    private final Logger logger = LoggerFactory.getLogger(ClubAdminController.class);
    private final ClubAdminService clubAdminService;

    public ClubAdminController(final ClubAdminService clubAdminService) {
        this.clubAdminService = clubAdminService;
    }

    @RolesAllowed(Roles.ADMIN)
    @PostMapping(value = "/clubadmins",
            consumes = MediaType.APPLICATION_JSON_VALUE,
            produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ClubAdminData> createClubAdmin(RequestEntity<ClubAdminData> clubAdminDataRequestEntity,
                                                         Principal principal) {
        ClubAdminData clubAdminData = Objects.requireNonNull(clubAdminDataRequestEntity).getBody();
        Objects.requireNonNull(clubAdminData);
        logger.info("User %s, POST on /clubadmins, body: %s".formatted(principal.getName(), clubAdminData));
        return ResponseEntity
                .status(HttpStatus.CREATED)
                .body(clubAdminService.createClubAdmin(clubAdminData));
    }

    @RolesAllowed(Roles.ADMIN)
    @DeleteMapping(value = "/clubadmins", consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<Void> deleteClubAdmin(RequestEntity<ClubAdminData> clubAdminDataRequestEntity,
                                                Principal principal) {
        ClubAdminData clubAdminData = Objects.requireNonNull(clubAdminDataRequestEntity).getBody();
        Objects.requireNonNull(clubAdminData);
        logger.info("User %s, DELETE on /clubadmins, body: %s".formatted(principal.getName(), clubAdminData));
        clubAdminService.deleteClubAdmin(clubAdminData);
        return ResponseEntity.noContent().build();
    }
}
