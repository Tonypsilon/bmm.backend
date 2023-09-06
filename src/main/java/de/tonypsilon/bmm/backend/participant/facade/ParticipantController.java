package de.tonypsilon.bmm.backend.participant.facade;

import de.tonypsilon.bmm.backend.exception.BadDataException;
import de.tonypsilon.bmm.backend.participant.data.ParticipantCreationData;
import de.tonypsilon.bmm.backend.participant.data.ParticipantData;
import de.tonypsilon.bmm.backend.participant.data.ParticipantsCreationData;
import de.tonypsilon.bmm.backend.participant.service.ParticipantService;
import de.tonypsilon.bmm.backend.security.rnr.Roles;
import de.tonypsilon.bmm.backend.security.rnr.service.AuthorizationService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.*;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.annotation.security.RolesAllowed;
import java.security.Principal;
import java.util.Collection;
import java.util.Objects;

@RestController
public class ParticipantController {

    private final Logger logger = LoggerFactory.getLogger(ParticipantController.class);
    private final ParticipantService participantService;
    private final AuthorizationService authorizationService;

    public ParticipantController(final ParticipantService participantService,
                                 final AuthorizationService authorizationService) {
        this.participantService = participantService;
        this.authorizationService = authorizationService;
    }

    @RolesAllowed(Roles.CLUB_ADMIN)
    @PostMapping(value = "/participants",
            consumes = MediaType.APPLICATION_JSON_VALUE,
            produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ParticipantData> createParticipant(
            RequestEntity<ParticipantCreationData> creationDataRequestEntity,
            Principal principal) {
        ParticipantCreationData creationData = Objects.requireNonNull(creationDataRequestEntity).getBody();
        Objects.requireNonNull(creationData);
        logger.info("User %s, POST on /participants, body: %s".formatted(principal.getName(), creationData));
        authorizationService.verifyUserIsClubAdminOfTeam(principal.getName(),
                Objects.requireNonNull(creationData.teamId()));
        return ResponseEntity
                .status(HttpStatus.CREATED)
                .body(participantService.createParticipant(creationData));
    }

    @RolesAllowed(Roles.CLUB_ADMIN)
    @PostMapping(value = "/teams/{teamId}/participants",
            consumes = MediaType.APPLICATION_JSON_VALUE,
            produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<Collection<ParticipantData>> createParticipants(
            RequestEntity<ParticipantsCreationData> creationDataRequestEntity,
            @PathVariable Long teamId,
            Principal principal) {
        ParticipantsCreationData creationData = Objects.requireNonNull(creationDataRequestEntity).getBody();
        Objects.requireNonNull(creationData);
        logger.info("User %s, POST on /teams/%s/participants, body: %s"
                .formatted(principal.getName(), teamId, creationData));
        if(!creationData.teamId().equals(teamId)) {
            throw new BadDataException("Die Mannschafts-ID aus dem Body passt nicht zur URL!");
        }
        authorizationService.verifyUserIsClubAdminOfTeam(principal.getName(),
                Objects.requireNonNull(creationData.teamId()));
        return ResponseEntity
                .status(HttpStatus.CREATED)
                .body(participantService.createValidParticipantConfigurationForTeam(creationData));
    }

}
