package de.tonypsilon.bmm.backend.participant.facade;

import de.tonypsilon.bmm.backend.participant.data.ParticipantCreationData;
import de.tonypsilon.bmm.backend.participant.data.ParticipantData;
import de.tonypsilon.bmm.backend.participant.data.ParticipantsCreationData;
import de.tonypsilon.bmm.backend.participant.service.ParticipantService;
import de.tonypsilon.bmm.backend.security.rnr.Roles;
import de.tonypsilon.bmm.backend.security.rnr.service.AuthorizationService;
import org.springframework.http.*;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.annotation.security.RolesAllowed;
import java.security.Principal;
import java.util.Collection;
import java.util.Objects;

@RestController
public class ParticipantController {

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
            Principal principal) {
        ParticipantsCreationData creationData = Objects.requireNonNull(creationDataRequestEntity).getBody();
        Objects.requireNonNull(creationData);
        authorizationService.verifyUserIsClubAdminOfTeam(principal.getName(),
                Objects.requireNonNull(creationData.teamId()));
        return ResponseEntity
                .status(HttpStatus.CREATED)
                .body(participantService.createValidParticipantConfigurationForTeam(creationData));
    }



}
