package de.tonypsilon.bmm.backend.club.facade;

import de.tonypsilon.bmm.backend.club.data.ClubCreationData;
import de.tonypsilon.bmm.backend.club.data.ClubData;
import de.tonypsilon.bmm.backend.club.service.ClubService;
import de.tonypsilon.bmm.backend.security.rnr.Roles;
import javax.annotation.security.RolesAllowed;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.RequestEntity;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RestController;

import java.security.Principal;
import java.util.Collection;
import java.util.Objects;

@RestController
public class ClubController {

    private final Logger logger = LoggerFactory.getLogger(ClubController.class);
    private final ClubService clubService;

    public ClubController(final ClubService clubService) {
        this.clubService = clubService;
    }

    @RolesAllowed(Roles.ADMIN)
    @GetMapping(value = "/clubs", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<Collection<ClubData>> getAllClubs() {
        return ResponseEntity
                .ok(clubService.getAllClubs());
    }

    @RolesAllowed(Roles.ADMIN)
    @PostMapping(value = "/clubs",
    consumes = MediaType.APPLICATION_JSON_VALUE,
    produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ClubData> createClub(RequestEntity<ClubCreationData> clubCreationDataRequestEntity,
                                               Principal principal) {
        ClubCreationData clubCreationData = Objects.requireNonNull(clubCreationDataRequestEntity).getBody();
        Objects.requireNonNull(clubCreationData);
        logger.info("User %s, POST on /clubs, body: %s".formatted(principal.getName(), clubCreationData));
        return ResponseEntity
                .status(HttpStatus.CREATED)
                .body(clubService.createClub(clubCreationData));
    }

}
