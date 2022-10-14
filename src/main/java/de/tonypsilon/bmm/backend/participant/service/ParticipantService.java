package de.tonypsilon.bmm.backend.participant.service;

import de.tonypsilon.bmm.backend.exception.AlreadyExistsException;
import de.tonypsilon.bmm.backend.exception.BmmException;
import de.tonypsilon.bmm.backend.exception.NotFoundException;
import de.tonypsilon.bmm.backend.exception.SeasonStageException;
import de.tonypsilon.bmm.backend.organization.service.OrganizationService;
import de.tonypsilon.bmm.backend.participant.data.Participant;
import de.tonypsilon.bmm.backend.participant.data.ParticipantCreationData;
import de.tonypsilon.bmm.backend.participant.data.ParticipantData;
import de.tonypsilon.bmm.backend.participant.data.ParticipantRepository;
import de.tonypsilon.bmm.backend.participationeligibility.service.ParticipationEligibilityService;
import de.tonypsilon.bmm.backend.season.service.SeasonService;
import de.tonypsilon.bmm.backend.season.service.SeasonStage;
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
    private final SeasonService seasonService;
    private final OrganizationService organizationService;

    public ParticipantService(ParticipantRepository participantRepository,
                              TeamService teamService,
                              ParticipationEligibilityService participationEligibilityService,
                              SeasonService seasonService,
                              OrganizationService organizationService) {
        this.participantRepository = participantRepository;
        this.teamService = teamService;
        this.participationEligibilityService = participationEligibilityService;
        this.seasonService = seasonService;
        this.organizationService = organizationService;
    }

    @Transactional
    public Collection<ParticipantData> createValidParticipantConfigurationForTeam(
            Long teamId, Collection<ParticipantCreationData> participantsCreationData) {
        if(Boolean.FALSE.equals(teamService.existsById(teamId))) {
            throw new NotFoundException("Es gibt keine Mannschaft mit der ID %d!".formatted(teamId));
        }
        Collection<ParticipantData> createdParticipants = participantsCreationData
                .stream()
                .map(this::createSingleParticipantFromGivenCollection)
                .toList();
        validateParticipantsNumbersOfTeam(teamId);
        return createdParticipants;
    }

    private ParticipantData createSingleParticipantFromGivenCollection(ParticipantCreationData participantCreationData) {
        validateParticipantCreationData(participantCreationData);
        if(seasonService.getStageOfSeason(
                organizationService.getSeasonIdOfOrganization(
                        teamService.getTeamById(participantCreationData.teamId()).organizationId()))
                != SeasonStage.REGISTRATION) {
            throw new SeasonStageException("In dieser Saisonphase kann keine Mannschaft mit Teilnehmern befüllt werden!");
        }
        // Number sequence validation is done at the end (and not here) so that the order of creation from
        // the given collection does not matter. Failure results in rollback anyway.
        participantRepository.save(createParticipantFromParticipantCreationData(participantCreationData));
        return participantToParticipantData(participantRepository.getByTeamIdAndNumber(
                participantCreationData.teamId(), participantCreationData.number())
        );
    }

    @Transactional
    public ParticipantData addParticipantToTeam(ParticipantCreationData participantCreationData) {
        validateParticipantCreationData(participantCreationData);
        if(!List.of(SeasonStage.REGISTRATION, SeasonStage.RUNNING).contains(
                seasonService.getStageOfSeason(
                        organizationService.getSeasonIdOfOrganization(
                        teamService.getTeamById(participantCreationData.teamId()).organizationId()))
        )) {
            throw new SeasonStageException(
                    "In dieser Saisonphase können keine Teilnehmer zu einer Mannschaft hinzugefügt werden!");
        }
        participantRepository.save(createParticipantFromParticipantCreationData(participantCreationData));
        validateParticipantsNumbersOfTeam(participantCreationData.teamId());
        return participantToParticipantData(participantRepository.getByTeamIdAndNumber(
                participantCreationData.teamId(), participantCreationData.number())
        );
    }

    @Transactional
    public void deleteParticipant(Long participantId) {
        Participant participantToDelete = participantRepository.findById(participantId).orElseThrow(
                        () -> new NotFoundException("Es gibt keinen Teilnehmer mit der ID %d!".formatted(participantId))
        );
        if(!List.of(SeasonStage.REGISTRATION)
                .contains(seasonService.getStageOfSeason(
                        organizationService.getSeasonIdOfOrganization(
                        teamService.getTeamById(participantToDelete.getTeamId()).organizationId()))
                )
        ) {
            throw new SeasonStageException("In dieser Saisonphase kann kein Teilnehmer entfernt werden!");
        }
        participantRepository.delete(participantToDelete);
        validateParticipantsNumbersOfTeam(participantToDelete.getTeamId());
    }

    private void validateParticipantsNumbersOfTeam(Long teamId) {
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

    private void validateParticipantCreationData(ParticipantCreationData participantCreationData) {
        if(Boolean.FALSE.equals(participationEligibilityService.existsById(participantCreationData.participationEligibilityId()))) {
            throw new NotFoundException("Es gibt keine Spielberechtigung mit der ID %d!"
                    .formatted(participantCreationData.participationEligibilityId()));
        }
        if(Boolean.TRUE.equals(participantRepository.existsByParticipationEligibilityId(
                participantCreationData.participationEligibilityId()))) {
            throw new AlreadyExistsException("Es gibt bereits einen Teilnehmer für die Spielberechtigung mit der ID %d!"
                    .formatted(participantCreationData.participationEligibilityId()));
        }
        if(Boolean.TRUE.equals(participantRepository.existsByTeamIdAndNumber(
                participantCreationData.teamId(), participantCreationData.number()))) {
            throw new AlreadyExistsException(
                    "Es gibt für die Mannschaft mit der ID %d bereits einen Teilnehmer mit Nummer %d!"
                            .formatted(participantCreationData.teamId(), participantCreationData.number()));
        }
    }

    private Participant createParticipantFromParticipantCreationData(ParticipantCreationData participantCreationData) {
        Participant participant = new Participant();
        participant.setTeamId(participantCreationData.teamId());
        participant.setParticipationEligibilityId(participantCreationData.participationEligibilityId());
        participant.setNumber(participantCreationData.number());
        return participant;
    }

    private ParticipantData participantToParticipantData(Participant participant) {
        return new ParticipantData(participant.getId(),
                participant.getTeamId(),
                participant.getParticipationEligibilityId(),
                participant.getNumber());
    }

}
