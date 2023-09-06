package de.tonypsilon.bmm.backend.participationeligibility.facade;

import de.tonypsilon.bmm.backend.participationeligibility.data.ParticipationEligibilityCreationData;
import de.tonypsilon.bmm.backend.participationeligibility.data.ParticipationEligibilityData;
import de.tonypsilon.bmm.backend.participationeligibility.service.ParticipationEligibilityService;
import de.tonypsilon.bmm.backend.security.rnr.Roles;
import de.tonypsilon.bmm.backend.security.rnr.service.AuthorizationService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.*;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.annotation.security.RolesAllowed;
import java.security.Principal;
import java.util.List;
import java.util.Objects;

@RestController
public class ParticipationEligibilityController {

    private final Logger logger = LoggerFactory.getLogger(ParticipationEligibilityController.class);
    private final ParticipationEligibilityService participationEligibilityService;
    private final AuthorizationService authorizationService;

    public ParticipationEligibilityController(final ParticipationEligibilityService participationEligibilityService,
                                              final AuthorizationService authorizationService) {
        this.participationEligibilityService = participationEligibilityService;
        this.authorizationService = authorizationService;
    }

    @RolesAllowed(Roles.SEASON_ADMIN)
    @PostMapping(value = "/participationeligibilities",
            consumes = MediaType.APPLICATION_JSON_VALUE,
            produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ParticipationEligibilityData> createParticipationEligibility(
            RequestEntity<ParticipationEligibilityCreationData> creationDataRequestEntity,
            Principal principal) {
        ParticipationEligibilityCreationData creationData = Objects.requireNonNull(creationDataRequestEntity).getBody();
        Objects.requireNonNull(creationData);
        logger.info("User %s, POST on /participationeligibilities, body: %s"
                .formatted(principal.getName(), creationData));
        authorizationService.verifyUserIsSeasonAdminOfSeason(principal.getName(),
                Objects.requireNonNull(creationData.seasonId()));
        return ResponseEntity
                .status(HttpStatus.CREATED)
                .body(participationEligibilityService.createParticipationEligibility(creationData));
    }

    @Transactional
    @RolesAllowed(Roles.SEASON_ADMIN)
    @PostMapping(value = "/seasons/{seasonId}/participationeligibilities",
            consumes = MediaType.APPLICATION_JSON_VALUE,
            produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<List<ParticipationEligibilityData>> createParticipationEligibilitiesForSeason(
            RequestEntity<List<ParticipationEligibilityCreationData>> participationEligibilitiesRequestEntity,
            Principal principal,
            @PathVariable Long seasonId) {
        authorizationService.verifyUserIsSeasonAdminOfSeason(principal.getName(), Objects.requireNonNull(seasonId));
        List<ParticipationEligibilityCreationData> participationEligibilityCreationDataList =
                Objects.requireNonNull(participationEligibilitiesRequestEntity).getBody();
        Objects.requireNonNull(participationEligibilityCreationDataList);
        logger.info("User %s, POST on /seasons/%s/participationeligibilities, body: %s"
                .formatted(principal.getName(), seasonId, participationEligibilityCreationDataList));
        return ResponseEntity
                .status(HttpStatus.CREATED)
                .body(participationEligibilityCreationDataList.stream()
                        .filter(Objects::nonNull)
                        .map(participationEligibilityService::createParticipationEligibility)
                        .toList());
    }

}
