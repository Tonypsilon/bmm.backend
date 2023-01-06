package de.tonypsilon.bmm.backend.security.rnr.facade;

import de.tonypsilon.bmm.backend.security.rnr.Roles;
import de.tonypsilon.bmm.backend.security.rnr.data.ClubAdminData;
import de.tonypsilon.bmm.backend.security.rnr.service.ClubAdminService;
import org.springframework.http.*;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.annotation.security.RolesAllowed;
import java.util.Objects;

@RestController
public class ClubAdminController {

    private final ClubAdminService clubAdminService;

    public ClubAdminController(final ClubAdminService clubAdminService) {
        this.clubAdminService = clubAdminService;
    }

    @RolesAllowed(Roles.ADMIN)
    @PostMapping(value = "/clubadmins",
            consumes = MediaType.APPLICATION_JSON_VALUE,
            produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ClubAdminData> createClubAdmin(
            RequestEntity<ClubAdminData> clubAdminDataRequestEntity) {
        return ResponseEntity
                .status(HttpStatus.CREATED)
                .body(clubAdminService.createClubAdmin(
                        Objects.requireNonNull(Objects.requireNonNull(clubAdminDataRequestEntity).getBody())));
    }

    @RolesAllowed(Roles.ADMIN)
    @DeleteMapping(value = "/clubadmins", consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<Void> deleteClubAdmin(RequestEntity<ClubAdminData> clubAdminDataRequestEntity) {
        clubAdminService.deleteClubAdmin(
                Objects.requireNonNull(Objects.requireNonNull(clubAdminDataRequestEntity).getBody()));
        return ResponseEntity.noContent().build();
    }
}
