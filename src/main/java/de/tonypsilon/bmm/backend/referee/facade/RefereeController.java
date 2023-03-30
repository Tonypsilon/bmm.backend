package de.tonypsilon.bmm.backend.referee.facade;

import de.tonypsilon.bmm.backend.referee.data.RefereeCreationData;
import de.tonypsilon.bmm.backend.referee.service.RefereeService;
import de.tonypsilon.bmm.backend.security.rnr.Roles;
import de.tonypsilon.bmm.backend.security.rnr.service.AuthorizationService;
import org.springframework.http.*;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.annotation.security.RolesAllowed;
import java.security.Principal;
import java.util.Objects;

@RestController
public class RefereeController {

    private final RefereeService refereeService;
    private final AuthorizationService authorizationService;

    public RefereeController(final RefereeService refereeService,
                             final AuthorizationService authorizationService) {
        this.refereeService = refereeService;
        this.authorizationService = authorizationService;
    }

    @RolesAllowed(Roles.SEASON_ADMIN)
    @PostMapping(value = "/referees",
            consumes = MediaType.APPLICATION_JSON_VALUE,
            produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<RefereeExternalData> createReferee(
            RequestEntity<RefereeCreationData> creationDataRequestEntity,
            Principal principal) {
        RefereeCreationData creationData = Objects.requireNonNull(creationDataRequestEntity).getBody();
        Objects.requireNonNull(creationData);
        authorizationService.verifyUserIsSeasonAdminOfSeason(principal.getName(),
                Objects.requireNonNull(creationData.seasonId()));
        return ResponseEntity
                .status(HttpStatus.CREATED)
                .body(refereeService.refereeDataToRefereeExternalData(
                        refereeService.createReferee(creationData)));

    }
}
