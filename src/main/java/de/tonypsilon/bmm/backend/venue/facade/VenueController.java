package de.tonypsilon.bmm.backend.venue.facade;

import de.tonypsilon.bmm.backend.security.rnr.Roles;
import de.tonypsilon.bmm.backend.security.rnr.service.AuthorizationService;
import de.tonypsilon.bmm.backend.venue.data.VenueCreationData;
import de.tonypsilon.bmm.backend.venue.data.VenueData;
import de.tonypsilon.bmm.backend.venue.service.VenueService;
import org.springframework.http.*;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.annotation.security.RolesAllowed;
import java.security.Principal;
import java.util.Objects;
import java.util.Set;

@RestController
public class VenueController {

     private final VenueService venueService;
     private final AuthorizationService authorizationService;

     public VenueController(final VenueService venueService,
                            final AuthorizationService authorizationService) {
         this.venueService = venueService;
         this.authorizationService = authorizationService;
     }

    @RolesAllowed(Roles.CLUB_ADMIN)
    @PostMapping(value = "/venues", consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<VenueData> createVenue(RequestEntity<VenueCreationData> venueCreationDataRequestEntity,
                                                 Principal principal) {
         VenueCreationData venueCreationData = Objects.requireNonNull(venueCreationDataRequestEntity.getBody());
         authorizationService.verifyUserIsClubAdminOfAnyClub(principal.getName(),
                 Set.of(Objects.requireNonNull(venueCreationData.clubId())));
         return ResponseEntity
                 .status(HttpStatus.CREATED)
                 .body(venueService.createVenue(venueCreationData));
    }
}
