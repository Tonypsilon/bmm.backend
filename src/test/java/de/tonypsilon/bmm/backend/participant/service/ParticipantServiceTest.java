package de.tonypsilon.bmm.backend.participant.service;

import de.tonypsilon.bmm.backend.exception.*;
import de.tonypsilon.bmm.backend.organization.service.OrganizationService;
import de.tonypsilon.bmm.backend.participant.data.*;
import de.tonypsilon.bmm.backend.participationeligibility.service.ParticipationEligibilityService;
import de.tonypsilon.bmm.backend.season.service.SeasonService;
import de.tonypsilon.bmm.backend.season.service.SeasonStage;
import de.tonypsilon.bmm.backend.team.data.TeamData;
import de.tonypsilon.bmm.backend.team.service.TeamService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.Collection;
import java.util.List;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.*;

class ParticipantServiceTest {

    private final ParticipantRepository participantRepository= mock(ParticipantRepository.class);
    private final TeamService teamService = mock(TeamService.class);
    private final ParticipationEligibilityService participationEligibilityService = mock(
            ParticipationEligibilityService.class);
    private final SeasonService seasonService = mock(SeasonService.class);
    private final OrganizationService organizationService = mock(OrganizationService.class);
    private ParticipantService participantService;
    private final ParticipantData participantData1 = new ParticipantData(1L, 1L, 2L, 1);
    private final ParticipantData participantData2 = new ParticipantData(2L, 1L, 1L, 2);
    private Participant participant1, participant2;
    private final ParticipantCreationData participantCreationData1 = new ParticipantCreationData(1L, 2L, 1);
    private final ParticipantCreationData participantCreationData2 = new ParticipantCreationData(1L, 1L, 2);

    @BeforeEach
    void setUp() {
        this.participantService = new ParticipantService(participantRepository,
                teamService,
                participationEligibilityService,
                seasonService,
                organizationService);
        participant1 = new Participant();
        participant1.setId(1L);
        participant1.setTeamId(1L);
        participant1.setParticipationEligibilityId(2L);
        participant1.setNumber(1);
        participant2 = new Participant();
        participant2.setId(2L);
        participant2.setTeamId(1L);
        participant2.setParticipationEligibilityId(1L);
        participant2.setNumber(2);
    }

    @Test
    void testCreateValidParticipantConfigurationForTeamOk() {
        when(teamService.existsById(1L)).thenReturn(Boolean.TRUE);
        when(teamService.getTeamDataById(1L)).thenReturn(new TeamData(
                1L, 3L, 4, 5L, null, "captain"));
        when(organizationService.getSeasonIdOfOrganization(3L)).thenReturn(2L);
        when(seasonService.getStageOfSeason(2L)).thenReturn(SeasonStage.REGISTRATION);
        when(participationEligibilityService.existsById(1L)).thenReturn(Boolean.TRUE);
        when(participationEligibilityService.existsById(2L)).thenReturn(Boolean.TRUE);
        when(participantRepository.existsByParticipationEligibilityId(1L)).thenReturn(Boolean.FALSE);
        when(participantRepository.existsByParticipationEligibilityId(2L)).thenReturn(Boolean.FALSE);
        when(participantRepository.existsByTeamIdAndNumber(1L, 1)).thenReturn(Boolean.FALSE);
        when(participantRepository.existsByTeamIdAndNumber(1L, 2)).thenReturn(Boolean.FALSE);
        when(participantRepository.getByTeamIdAndNumber(1L, 1)).thenReturn(participant1);
        when(participantRepository.getByTeamIdAndNumber(1L, 2)).thenReturn(participant2);
        when(participantRepository.findByTeamId(1L)).thenReturn(List.of(participant1, participant2));

        Collection<ParticipantData> actual = participantService.createValidParticipantConfigurationForTeam(
                new ParticipantsCreationData(1L, List.of(participantCreationData1, participantCreationData2)));
        assertThat(actual).hasSize(2);
        assertTrue(actual.containsAll(List.of(participantData1, participantData2)));

        verify(participantRepository, times(1)).save(argThat(
                participant -> participant.getTeamId().equals(1L)
                && participant.getParticipationEligibilityId().equals(2L)
                && participant.getNumber().equals(1)
        ));
        verify(participantRepository, times(1)).save(argThat(
                participant -> participant.getTeamId().equals(1L)
                        && participant.getParticipationEligibilityId().equals(1L)
                        && participant.getNumber().equals(2)
        ));


    }

    @Test
    void testCreateValidParticipantConfigurationForTeamTeamDoesNotExist() {
        when(teamService.existsById(1L)).thenReturn(Boolean.FALSE);
        NotFoundException actualException = assertThrows(NotFoundException.class,
                () -> participantService.createValidParticipantConfigurationForTeam(
                        new ParticipantsCreationData(1L, List.of(participantCreationData1, participantCreationData2))));
        assertThat(actualException.getMessage())
                .isEqualTo("Es gibt keine Mannschaft mit der ID 1!");
    }

    @Test
    void testCreateValidParticipantConfigurationForTeamInvalidNumbers() {
        when(teamService.existsById(1L)).thenReturn(Boolean.TRUE);
        when(teamService.getTeamDataById(1L)).thenReturn(new TeamData(
                1L, 3L, 4, 5L, null, "captain"));
        when(participationEligibilityService.existsById(1L)).thenReturn(Boolean.TRUE);
        when(organizationService.getSeasonIdOfOrganization(3L)).thenReturn(2L);
        when(seasonService.getStageOfSeason(2L)).thenReturn(SeasonStage.REGISTRATION);
        when(participantRepository.existsByParticipationEligibilityId(1L)).thenReturn(Boolean.FALSE);
        when(participantRepository.existsByTeamIdAndNumber(1L, 2)).thenReturn(Boolean.FALSE);
        when(participantRepository.findByTeamId(1L)).thenReturn(List.of(participant2));
        when(participantRepository.getByTeamIdAndNumber(1L, 2)).thenReturn(participant2);
        BmmException actualException = assertThrows(BmmException.class,
                () -> participantService.createValidParticipantConfigurationForTeam(
                        new ParticipantsCreationData(1L, List.of(participantCreationData2)))
        );
        assertThat(actualException.getMessage())
                .isEqualTo("Die Spielernummern für die Mannschaft mit ID 1 sind nicht gültig: [2]");
    }

    @Test
    void testCreateValidParticipantConfigurationForTeamWrongSeasonStage() {
        when(teamService.existsById(1L)).thenReturn(Boolean.TRUE);
        when(teamService.getTeamDataById(1L)).thenReturn(new TeamData(
                1L, 3L, 4, 5L, null, "captain"));
        when(participationEligibilityService.existsById(2L)).thenReturn(Boolean.TRUE);
        when(organizationService.getSeasonIdOfOrganization(3L)).thenReturn(2L);
        when(seasonService.getStageOfSeason(2L)).thenReturn(SeasonStage.RUNNING);
        when(participantRepository.existsByParticipationEligibilityId(2L)).thenReturn(Boolean.FALSE);
        when(participantRepository.existsByTeamIdAndNumber(1L, 1)).thenReturn(Boolean.FALSE);
        SeasonStageException actualException = assertThrows(SeasonStageException.class,
                () -> participantService.createValidParticipantConfigurationForTeam(
                        new ParticipantsCreationData(1L, List.of(participantCreationData1)))
        );
        assertThat(actualException.getMessage())
                .isEqualTo("In dieser Saisonphase kann keine Mannschaft mit Teilnehmern befüllt werden!");
    }

    @Test
    void addParticipantToTeamOk() {
        when(participationEligibilityService.existsById(1L)).thenReturn(Boolean.TRUE);
        when(participantRepository.existsByParticipationEligibilityId(1L)).thenReturn(Boolean.FALSE);
        when(participantRepository.existsByTeamIdAndNumber(1L, 2)).thenReturn(Boolean.FALSE);
        when(teamService.getTeamDataById(1L)).thenReturn(new TeamData(
                1L, 3L, 4, 5L, null, "captain"));
        when(teamService.existsById(1L)).thenReturn(Boolean.TRUE);
        when(organizationService.getSeasonIdOfOrganization(3L)).thenReturn(2L);
        when(seasonService.getStageOfSeason(2L)).thenReturn(SeasonStage.RUNNING);
        when(participantRepository.findByTeamId(1L)).thenReturn(List.of(participant1, participant2));
        when(participantRepository.getByTeamIdAndNumber(1L, 2)).thenReturn(participant2);

        ParticipantData actual = participantService.addParticipantToTeam(participantCreationData2);
        assertThat(actual).isEqualTo(participantData2);

        verify(participantRepository, times(1)).save(argThat(
                participant -> participant.getTeamId().equals(1L)
                && participant.getParticipationEligibilityId().equals(1L)
                && participant.getNumber().equals(2))
        );
    }

    @Test
    void testAddParticipantToTeamParticipationEligibilityDoesNotExist() {
        when(teamService.existsById(1L)).thenReturn(Boolean.TRUE);
        when(participationEligibilityService.existsById(1L)).thenReturn(Boolean.FALSE);

        NotFoundException actualException = assertThrows(NotFoundException.class,
                () -> participantService.addParticipantToTeam(participantCreationData2)
        );
        assertThat(actualException.getMessage())
                .isEqualTo("Es gibt keine Spielberechtigung mit der ID 1!");
    }

    @Test
    void testAddParticipantToTeamParticipantForParticipationEligibilityAlreadyExists() {
        when(participationEligibilityService.existsById(1L)).thenReturn(Boolean.TRUE);
        when(participantRepository.existsByParticipationEligibilityId(1L)).thenReturn(Boolean.TRUE);
        when(teamService.existsById(1L)).thenReturn(Boolean.TRUE);

        AlreadyExistsException actualException = assertThrows(AlreadyExistsException.class,
                () -> participantService.addParticipantToTeam(participantCreationData2)
        );
        assertThat(actualException.getMessage())
                .isEqualTo("Es gibt bereits einen Teilnehmer für die Spielberechtigung mit der ID 1!");
    }

    @Test
    void testAddParticipantToTeamNumberNotAvailableForTeam() {
        when(participationEligibilityService.existsById(1L)).thenReturn(Boolean.TRUE);
        when(participantRepository.existsByParticipationEligibilityId(1L)).thenReturn(Boolean.FALSE);
        when(participantRepository.existsByTeamIdAndNumber(1L, 2)).thenReturn(Boolean.TRUE);
        when(teamService.existsById(1L)).thenReturn(Boolean.TRUE);

        AlreadyExistsException actualException = assertThrows(AlreadyExistsException.class,
                () -> participantService.addParticipantToTeam(participantCreationData2)
        );
        assertThat(actualException.getMessage())
                .isEqualTo("Es gibt für die Mannschaft mit der ID 1 bereits einen Teilnehmer mit Nummer 2!");
    }

    @Test
    void testAddParticipantToTeamWrongSeasonStage() {
        when(participationEligibilityService.existsById(1L)).thenReturn(Boolean.TRUE);
        when(participantRepository.existsByParticipationEligibilityId(1L)).thenReturn(Boolean.FALSE);
        when(participantRepository.existsByTeamIdAndNumber(1L, 2)).thenReturn(Boolean.FALSE);
        when(teamService.existsById(1L)).thenReturn(Boolean.TRUE);
        when(teamService.getTeamDataById(1L)).thenReturn(new TeamData(
                1L, 3L, 4, 5L, null, "captain"));
        when(organizationService.getSeasonIdOfOrganization(3L)).thenReturn(2L);
        when(seasonService.getStageOfSeason(2L)).thenReturn(SeasonStage.PREPARATION);

        SeasonStageException actualException = assertThrows(SeasonStageException.class,
                () -> participantService.addParticipantToTeam(participantCreationData2)
        );
        assertThat(actualException.getMessage())
                .isEqualTo("In dieser Saisonphase können keine Teilnehmer zu einer Mannschaft hinzugefügt werden!");
    }

    @Test
    void testAddParticipantToTeamInvalidNumber() {
        when(participationEligibilityService.existsById(1L)).thenReturn(Boolean.TRUE);
        when(participantRepository.existsByParticipationEligibilityId(1L)).thenReturn(Boolean.FALSE);
        when(participantRepository.existsByTeamIdAndNumber(1L, 2)).thenReturn(Boolean.FALSE);
        when(teamService.existsById(1L)).thenReturn(Boolean.TRUE);
        when(teamService.getTeamDataById(1L)).thenReturn(new TeamData(
                1L, 3L, 4, 5L, null, "captain"));
        when(organizationService.getSeasonIdOfOrganization(3L)).thenReturn(2L);
        when(seasonService.getStageOfSeason(2L)).thenReturn(SeasonStage.RUNNING);
        when(participantRepository.findByTeamId(1L)).thenReturn(List.of(participant2));

        BmmException actualException = assertThrows(BmmException.class,
                () -> participantService.addParticipantToTeam(participantCreationData2)
        );
        assertThat(actualException.getMessage())
                .isEqualTo("Die Spielernummern für die Mannschaft mit ID 1 sind nicht gültig: [2]");
    }

    @Test
    void testDeleteParticipantOk() {
        when(participantRepository.findById(2L)).thenReturn(Optional.of(participant2));
        when(teamService.getTeamDataById(1L)).thenReturn(new TeamData(
                1L, 3L, 4, 5L, null, "captain"));
        when(organizationService.getSeasonIdOfOrganization(3L)).thenReturn(2L);
        when(seasonService.getStageOfSeason(2L)).thenReturn(SeasonStage.REGISTRATION);
        when(participantRepository.findByTeamId(1L)).thenReturn(List.of(participant1));

        participantService.deleteParticipant(2L);

        verify(participantRepository, times(1)).delete(argThat(
                participant ->participant.getId().equals(2L)
                && participant.getParticipationEligibilityId().equals(1L)
                && participant.getTeamId().equals(1L)
                && participant.getNumber().equals(2))
        );
    }

    @Test
    void testDeleteParticipantThatDoesNotExist() {
        when(participantRepository.findById(2L)).thenReturn(Optional.empty());
        NotFoundException actualException = assertThrows(NotFoundException.class,
                () -> participantService.deleteParticipant(2L)
        );
        assertThat(actualException.getMessage())
                .isEqualTo("Es gibt keinen Teilnehmer mit der ID 2!");

    }

    @Test
    void testDeleteParticipantWrongSeasonStage() {
        when(participantRepository.findById(2L)).thenReturn(Optional.of(participant2));
        when(teamService.getTeamDataById(1L)).thenReturn(new TeamData(
                1L, 3L, 4, 5L, null, "captain"));
        when(organizationService.getSeasonIdOfOrganization(3L)).thenReturn(2L);
        when(seasonService.getStageOfSeason(2L)).thenReturn(SeasonStage.ARCHIVED);
        SeasonStageException actualException = assertThrows(SeasonStageException.class,
                () -> participantService.deleteParticipant(2L)
        );
        assertThat(actualException.getMessage())
                .isEqualTo("In dieser Saisonphase kann kein Teilnehmer entfernt werden!");
    }

    @Test
    void testDeleteParticipantWrongNumber() {
        when(participantRepository.findById(1L)).thenReturn(Optional.of(participant1));
        when(organizationService.getSeasonIdOfOrganization(3L)).thenReturn(2L);
        when(teamService.getTeamDataById(1L)).thenReturn(new TeamData(
                1L, 3L, 4, 5L, null, "captain"));
        when(seasonService.getStageOfSeason(2L)).thenReturn(SeasonStage.REGISTRATION);
        when(participantRepository.findByTeamId(1L)).thenReturn(List.of(participant2));
        BmmException actualException = assertThrows(BmmException.class,
                () -> participantService.deleteParticipant(1L)
        );
        assertThat(actualException.getMessage())
                .isEqualTo("Die Spielernummern für die Mannschaft mit ID 1 sind nicht gültig: [2]");
    }


}