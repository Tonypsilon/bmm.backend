package de.tonypsilon.bmm.backend.participant.service;

import de.tonypsilon.bmm.backend.exception.AlreadyExistsException;
import de.tonypsilon.bmm.backend.exception.BmmException;
import de.tonypsilon.bmm.backend.exception.NotFoundException;
import de.tonypsilon.bmm.backend.participant.data.Participant;
import de.tonypsilon.bmm.backend.participant.data.ParticipantCreationData;
import de.tonypsilon.bmm.backend.participant.data.ParticipantData;
import de.tonypsilon.bmm.backend.participant.data.ParticipantRepository;
import de.tonypsilon.bmm.backend.participationeligibility.service.ParticipationEligibilityService;
import de.tonypsilon.bmm.backend.team.service.TeamService;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Collection;
import java.util.List;

@Service
public class ParticipantService {

    private final ParticipantRepository participantRepository;
    private final TeamService teamService;
    private final ParticipationEligibilityService participationEligibilityService;

    public ParticipantService(ParticipantRepository participantRepository,
                              TeamService teamService,
                              ParticipationEligibilityService participationEligibilityService) {
        this.participantRepository = participantRepository;
        this.teamService = teamService;
        this.participationEligibilityService = participationEligibilityService;
    }

    @Transactional
    public Collection<ParticipantData> createValidParticipantConfigurationForTeam(
            Long teamId, Collection<ParticipantCreationData> participantsCreateData) {
        if(!teamService.existsById(teamId)) {
            throw new NotFoundException("Es gibt keine Mannschaft mit der ID %d!".formatted(teamId));
        }
        Collection<ParticipantData> createdParticipants = participantsCreateData
                .stream()
                .map(this::createParticipant)
                .toList();
        validateParticipantsOfTeam(teamId);
        return createdParticipants;
    }

    private ParticipantData createParticipant(ParticipantCreationData participantCreationData) {
        if(!participationEligibilityService.existsById(participantCreationData.participationEligibilityId())) {
            throw new NotFoundException("Es gibt keine Spielberechtigung mit der ID %d!"
                    .formatted(participantCreationData.participationEligibilityId()));
        }
        if(participantRepository.existsByParticipationEligibilityId(
                participantCreationData.participationEligibilityId())) {
            throw new AlreadyExistsException("Es gibt bereits einen Teilnehmer für die Spielberechtigung mit der ID %d!"
                    .formatted(participantCreationData.participationEligibilityId()));
        }
        if(participantRepository.existsByTeamIdAndNumber(
                participantCreationData.teamId(), participantCreationData.number())) {
            throw new AlreadyExistsException(
                    "Es gibt für die Mannschaft mit der ID %d bereits einen Teilnehmer mit Nummer %d"
                            .formatted(participantCreationData.teamId(), participantCreationData.number()));
        }
        // Number sequence validation is done at the end (and not here) so that the order of creation from
        // the given collection does not matter. Failure results in rollback anyway.
        Participant participant = new Participant();
        participant.setTeamId(participantCreationData.teamId());
        participant.setParticipationEligibilityId(participantCreationData.participationEligibilityId());
        participant.setNumber(participantCreationData.number());
        participantRepository.save(participant);
        return participantToParticipantData(participantRepository.getByTeamIdAndNumber(
                participantCreationData.teamId(), participantCreationData.number())
        );
    }

    private void validateParticipantsOfTeam(Long teamId) {
        List<Integer> currentTeamNumbers = participantRepository.findByTeamId(teamId)
                .stream()
                .map(Participant::getNumber)
                .sorted()
                .toList();
        for (int i=0; i<currentTeamNumbers.size(); i++) {
            if (!currentTeamNumbers.get(i).equals(i+1)) {
                throw new BmmException("Die Spielernummern für die Mannschaft mit ID %d sind nicht gültig: "
                        .formatted(teamId)
                        + currentTeamNumbers.toString());
            }
        }
    }

    private ParticipantData participantToParticipantData(Participant participant) {
        return new ParticipantData(participant.getId(),
                participant.getTeamId(),
                participant.getParticipationEligibilityId(),
                participant.getNumber());
    }

}
