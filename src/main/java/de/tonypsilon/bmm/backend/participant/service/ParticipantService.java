package de.tonypsilon.bmm.backend.participant.service;

import de.tonypsilon.bmm.backend.participant.data.Participant;
import de.tonypsilon.bmm.backend.participant.data.ParticipantCreateData;
import de.tonypsilon.bmm.backend.participant.data.ParticipantData;
import de.tonypsilon.bmm.backend.participant.data.ParticipantRepository;
import de.tonypsilon.bmm.backend.team.service.TeamService;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Collection;
import java.util.List;

@Service
public class ParticipantService {

    private final ParticipantRepository participantRepository;
    private final TeamService teamService;

    public ParticipantService(ParticipantRepository participantRepository,
                              TeamService teamService) {
        this.participantRepository = participantRepository;
        this.teamService = teamService;
    }

    @Transactional
    public Collection<ParticipantData> createValidParticipantConfigurationForTeam(
            Long teamId, Collection<ParticipantCreateData> participantsCreateData) {
        Collection<ParticipantData> createdParticipants = participantsCreateData
                .stream()
                .map(this::createParticipant)
                .toList();
        validateParticipantsOfTeam(teamId);
        return createdParticipants;
    }

    private ParticipantData createParticipant(ParticipantCreateData participantCreateData) {
        return null;
    }

    private void validateParticipantsOfTeam(Long teamId) {

    }

    private ParticipantData participantToParticipantData(Participant participant) {
        return new ParticipantData(participant.getId(),
                participant.getTeamId(),
                participant.getParticipationEligibilityId(),
                participant.getNumber());
    }

}
