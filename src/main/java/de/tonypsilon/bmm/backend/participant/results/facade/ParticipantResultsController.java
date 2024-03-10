package de.tonypsilon.bmm.backend.participant.results.facade;

import de.tonypsilon.bmm.backend.participant.results.data.ParticipantResultsData;
import de.tonypsilon.bmm.backend.participant.results.service.ParticipantResultsService;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RestController;

import java.util.Objects;

@RestController
public class ParticipantResultsController {

    private final ParticipantResultsService participantResultsService;

    public ParticipantResultsController(ParticipantResultsService participantResultsService) {
        this.participantResultsService = participantResultsService;
    }

    @GetMapping("participants/{id}/results")
    public ResponseEntity<ParticipantResultsData> getResultsForParticipant(@PathVariable("id") String id) {
        return ResponseEntity
                .ok(participantResultsService.getResultsForParticipant(Objects.requireNonNull(Long.valueOf(id))));
    }
}
