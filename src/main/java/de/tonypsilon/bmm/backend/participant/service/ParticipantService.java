package de.tonypsilon.bmm.backend.participant.service;

import de.tonypsilon.bmm.backend.exception.AlreadyExistsException;
import de.tonypsilon.bmm.backend.exception.BmmException;
import de.tonypsilon.bmm.backend.exception.NotFoundException;
import de.tonypsilon.bmm.backend.exception.SeasonStageException;
import de.tonypsilon.bmm.backend.organization.service.OrganizationService;
import de.tonypsilon.bmm.backend.participant.data.*;
import de.tonypsilon.bmm.backend.participationeligibility.service.ParticipationEligibilityService;
import de.tonypsilon.bmm.backend.season.service.SeasonService;
import de.tonypsilon.bmm.backend.season.service.SeasonStage;
import de.tonypsilon.bmm.backend.team.data.TeamData;
import de.tonypsilon.bmm.backend.team.service.TeamService;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;
import java.util.stream.Stream;

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
    @NonNull
    public Collection<ParticipantData> createValidParticipantConfigurationForTeam(
            ParticipantsCreationData participantsCreationData) {
        if(Boolean.FALSE.equals(teamService.existsById(participantsCreationData.teamId()))) {
            throw new NotFoundException("Es gibt keine Mannschaft mit der ID %d!"
                    .formatted(participantsCreationData.teamId()));
        }
        Collection<ParticipantData> createdParticipants = participantsCreationData.participantCreationDataCollection()
                .stream()
                .map(this::createSingleParticipantFromGivenCollection)
                .toList();
        validateParticipantsNumbersOfTeam(participantsCreationData.teamId());
        return createdParticipants;
    }

    @Transactional
    @NonNull
    public ParticipantData createParticipant(@NonNull ParticipantCreationData participantCreationData) {
        validateParticipantCreationData(participantCreationData);
        if(seasonService.getStageOfSeason(
                organizationService.getSeasonIdOfOrganization(
                        teamService.getTeamDataById(participantCreationData.teamId()).organizationId()))
                != SeasonStage.REGISTRATION) {
            throw new SeasonStageException("In dieser Saisonphase kann keine Mannschaft mit Teilnehmern befüllt werden!");
        }
        participantRepository.save(createParticipantFromParticipantCreationData(participantCreationData));
        validateParticipantsNumbersOfTeam(participantCreationData.teamId());
        return participantToParticipantData(participantRepository.getByTeamIdAndNumber(
                participantCreationData.teamId(), participantCreationData.number())
        );
    }

    private ParticipantData createSingleParticipantFromGivenCollection(ParticipantCreationData participantCreationData) {
        validateParticipantCreationData(participantCreationData);
        if(seasonService.getStageOfSeason(
                organizationService.getSeasonIdOfOrganization(
                        teamService.getTeamDataById(participantCreationData.teamId()).organizationId()))
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
    @NonNull
    public ParticipantData addParticipantToTeam(@NonNull ParticipantCreationData participantCreationData) {
        validateParticipantCreationData(participantCreationData);
        if(!List.of(SeasonStage.REGISTRATION, SeasonStage.RUNNING).contains(
                seasonService.getStageOfSeason(
                        organizationService.getSeasonIdOfOrganization(
                        teamService.getTeamDataById(participantCreationData.teamId()).organizationId()))
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
    public void deleteParticipant(@NonNull Long participantId) {
        Participant participantToDelete = participantRepository.findById(participantId).orElseThrow(
                        () -> new NotFoundException("Es gibt keinen Teilnehmer mit der ID %d!".formatted(participantId))
        );
        if(SeasonStage.REGISTRATION !=
                seasonService.getStageOfSeason(
                        organizationService.getSeasonIdOfOrganization(
                        teamService.getTeamDataById(participantToDelete.getTeamId()).organizationId()))
        ) {
            throw new SeasonStageException("In dieser Saisonphase kann kein Teilnehmer entfernt werden!");
        }
        participantRepository.delete(participantToDelete);
        validateParticipantsNumbersOfTeam(participantToDelete.getTeamId());
    }

    @Transactional
    public void deleteParticipantsOfOrganization(@NonNull Long organizationId) {
        if(SeasonStage.REGISTRATION != seasonService.getStageOfSeason(
                organizationService.getSeasonIdOfOrganization(organizationId))) {
            throw new SeasonStageException("In dieser Saisonphase kann kein Teilnehmer entfernt werden!");
        }
        teamService.getTeamsOfOrganization(organizationId).stream()
                .map(TeamData::id)
                .map(participantRepository::findByTeamId)
                .forEach(participantRepository::deleteAll);
    }

    @NonNull
    public ParticipantData getParticipantById(Long participantId) {
        return participantRepository.findById(participantId)
                .map(this::participantToParticipantData)
                .orElseThrow(() -> new NotFoundException("Es gibt keinen Teilnehmer mit der ID %d!"
                        .formatted(participantId)));
    }

    @NonNull
    @Transactional
    public String getCodeOfParticipant(Long participantId) {
        ParticipantData participant = getParticipantById(participantId);
        TeamData teamData = teamService.getTeamDataById(participant.teamId());
        String leadingZeroTeams = teamData.number() <10 ? "0" : "";
        String leadingZeroParticipants = participant.number()<10 ? "0" : "";
        return leadingZeroTeams + teamData.number()
                + leadingZeroParticipants + participant.number();
    }

    @NonNull
    public List<ParticipantData> getParticipantsOfTeamOrderedByNumberAsc(Long teamId) {
        return participantRepository.getByTeamIdOrderByNumberAsc(teamId).stream()
                .map(this::participantToParticipantData)
                .toList();
    }

    @NonNull
    public List<ParticipantData> getParticipantsEligibleForTeam(Long teamId) {
        List<ParticipantData> eligibleParticipants = getParticipantsOfTeamOrderedByNumberAsc(teamId);
        Optional<List<ParticipantData>> followingTeamParticipants = teamService.findFollowingTeam(teamId)
                .map(teamData -> getParticipantsOfTeamOrderedByNumberAsc(teamData.id()).stream()
                        .filter(participantData -> participantData.number() <= 16)
                        .toList());
        return Stream.of(eligibleParticipants, followingTeamParticipants.orElse(Collections.emptyList()))
                .flatMap(List::stream)
                .toList();
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
                        + currentTeamNumbers);
            }
        }
    }

    private void validateParticipantCreationData(ParticipantCreationData participantCreationData) {
        if(Boolean.FALSE.equals(teamService.existsById(participantCreationData.teamId()))) {
            throw new NotFoundException("Es gibt keine Mannschaft mit der ID %d!"
                    .formatted(participantCreationData.teamId()));
        }
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

    @NonNull
    private ParticipantData participantToParticipantData(@NonNull Participant participant) {
        return new ParticipantData(participant.getId(),
                participant.getTeamId(),
                participant.getParticipationEligibilityId(),
                participant.getNumber());
    }

}
