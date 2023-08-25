package de.tonypsilon.bmm.backend.venue.facade;

import de.tonypsilon.bmm.backend.security.rnr.Roles;
import de.tonypsilon.bmm.backend.security.rnr.service.AuthorizationService;
import de.tonypsilon.bmm.backend.venue.data.VenueCreationData;
import de.tonypsilon.bmm.backend.venue.data.VenueData;
import de.tonypsilon.bmm.backend.venue.service.VenueService;
import org.springframework.http.*;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.bind.annotation.*;

import javax.annotation.security.RolesAllowed;
import java.security.Principal;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Stream;

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
    @PostMapping(value = "/venues",
            consumes = MediaType.APPLICATION_JSON_VALUE,
            produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<VenueData> createVenue(RequestEntity<VenueCreationData> venueCreationDataRequestEntity,
                                                 Principal principal) {
         VenueCreationData venueCreationData = Objects.requireNonNull(venueCreationDataRequestEntity.getBody());
         authorizationService.verifyUserIsClubAdminOfAnyClub(principal.getName(),
                 Set.of(Objects.requireNonNull(venueCreationData.clubId())));
         return ResponseEntity
                 .status(HttpStatus.CREATED)
                 .body(venueService.createVenue(venueCreationData));
    }

    @RolesAllowed(Roles.CLUB_ADMIN)
    @GetMapping(value = "/venues/club/{clubId}", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<List<VenueData>> getVenuesForClub(Principal principal,
                                                            @PathVariable Long clubId) {
        authorizationService.verifyUserIsClubAdminOfAnyClub(principal.getName(),
                Set.of(Objects.requireNonNull(clubId)));
        return ResponseEntity.ok(venueService.getVenuesForClub(clubId));
    }

    @Transactional
    @RolesAllowed(Roles.CLUB_ADMIN)
    @PutMapping(value = "/venues/club/{clubId}",
            consumes = MediaType.APPLICATION_JSON_VALUE,
            produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<List<VenueData>> putVenuesForClub(
            RequestEntity<List<VenueData>> venueDataForClubRequestEntity,
            Principal principal,
            @PathVariable Long clubId) {
         Objects.requireNonNull(venueDataForClubRequestEntity);
         authorizationService.verifyUserIsClubAdminOfAnyClub(principal.getName(),
                 Set.of(Objects.requireNonNull(clubId)));
         List<VenueData> venueDataForClub = Objects.requireNonNull(venueDataForClubRequestEntity.getBody()).stream()
                .filter(venueData -> venueData.clubId().equals(clubId))
                .toList();
         List<VenueData> updatedVenues = venueDataForClub.stream()
                .filter(venueData -> venueData.id() != null)
                .map(venueService::updateVenue)
                .toList();
         List<VenueData> createdVenues = venueDataForClub.stream()
                .filter(venueData -> venueData.id() == null)
                .map(venueData -> new VenueCreationData(venueData.clubId(), venueData.address(), venueData.hints()))
                .map(venueService::createVenue)
                .toList();
         return ResponseEntity
                .ok(Stream.of(createdVenues, updatedVenues).flatMap(List::stream).toList());
    }
}
