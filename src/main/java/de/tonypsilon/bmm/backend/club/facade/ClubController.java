package de.tonypsilon.bmm.backend.club.facade;

import de.tonypsilon.bmm.backend.club.data.ClubCreationData;
import de.tonypsilon.bmm.backend.club.data.ClubData;
import de.tonypsilon.bmm.backend.club.service.ClubService;
import de.tonypsilon.bmm.backend.security.rnr.Roles;
import javax.annotation.security.RolesAllowed;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.RequestEntity;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.Collection;

@RestController
public class ClubController {

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
    public ResponseEntity<ClubData> createClub(RequestEntity<ClubCreationData> clubCreationDataRequestEntity) {
        return ResponseEntity
                .status(HttpStatus.CREATED)
                .body(clubService.createClub(clubCreationDataRequestEntity.getBody()));
    }

}
