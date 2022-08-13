package de.tonypsilon.bmm.backend.participant.service;

import de.tonypsilon.bmm.backend.exception.BmmException;
import de.tonypsilon.bmm.backend.exception.NotFoundException;
import de.tonypsilon.bmm.backend.exception.SeasonStageException;
import de.tonypsilon.bmm.backend.participant.data.Participant;
import de.tonypsilon.bmm.backend.participant.data.ParticipantCreationData;
import de.tonypsilon.bmm.backend.participant.data.ParticipantData;
import de.tonypsilon.bmm.backend.participant.data.ParticipantRepository;
import de.tonypsilon.bmm.backend.participationeligibility.service.ParticipationEligibilityService;
import de.tonypsilon.bmm.backend.season.service.SeasonService;
import de.tonypsilon.bmm.backend.season.service.SeasonStage;
import de.tonypsilon.bmm.backend.team.data.TeamData;
import de.tonypsilon.bmm.backend.team.service.TeamService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.Collection;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

class ParticipantServiceTest {

    private final ParticipantRepository participantRepository= mock(ParticipantRepository.class);
    private final TeamService teamService = mock(TeamService.class);
    private final ParticipationEligibilityService participationEligibilityService = mock(
            ParticipationEligibilityService.class);
    private final SeasonService seasonService = mock(SeasonService.class);
    private ParticipantService participantService;
    private final ParticipantData participantData1 = new ParticipantData(1L, 1L, 2L, 1);
    private final ParticipantData participantData2 = new ParticipantData(2L, 1L, 1L, 2);
    private Participant participant1, participant2;
    private final ParticipantCreationData participantCreationData1 = new ParticipantCreationData(1L, 2L, 1);
    private final ParticipantCreationData participantCreationData2 = new ParticipantCreationData(1L, 1L, 2);

    @BeforeEach
    private void setUp() {
        this.participantService = new ParticipantService(participantRepository,
                teamService,
                participationEligibilityService,
                seasonService);
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
        when(teamService.getTeamById(1L)).thenReturn(new TeamData(1L, 2L, 3L, 4));
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
                1L, List.of(participantCreationData1, participantCreationData2));
        assertEquals(2, actual.size());
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
                        1L, List.of(participantCreationData1, participantCreationData2)));
        assertEquals("Es gibt keine Mannschaft mit der ID 1!", actualException.getMessage());
    }

    @Test
    void testCreateValidParticipantConfigurationForTeamInvalidNumbers() {
        when(teamService.existsById(1L)).thenReturn(Boolean.TRUE);
        when(teamService.getTeamById(1L)).thenReturn(new TeamData(1L, 2L, 3L, 4));
        when(participationEligibilityService.existsById(1L)).thenReturn(Boolean.TRUE);
        when(seasonService.getStageOfSeason(2L)).thenReturn(SeasonStage.REGISTRATION);
        when(participantRepository.existsByParticipationEligibilityId(1L)).thenReturn(Boolean.FALSE);
        when(participantRepository.existsByTeamIdAndNumber(1L, 2)).thenReturn(Boolean.FALSE);
        when(participantRepository.findByTeamId(1L)).thenReturn(List.of(participant2));
        when(participantRepository.getByTeamIdAndNumber(1L, 2)).thenReturn(participant2);
        BmmException actualException = assertThrows(BmmException.class,
                () -> participantService.createValidParticipantConfigurationForTeam(
                        1L, List.of(participantCreationData2))
        );
        assertEquals("Die Spielernummern für die Mannschaft mit ID 1 sind nicht gültig: [2]"
                , actualException.getMessage());
    }

    @Test
    void testCreateValidParticipantConfigurationForTeamWrongSeasonStage() {
        when(teamService.existsById(1L)).thenReturn(Boolean.TRUE);
        when(teamService.getTeamById(1L)).thenReturn(new TeamData(1L, 2L, 3L, 4));
        when(participationEligibilityService.existsById(2L)).thenReturn(Boolean.TRUE);
        when(seasonService.getStageOfSeason(2L)).thenReturn(SeasonStage.RUNNING);
        when(participantRepository.existsByParticipationEligibilityId(2L)).thenReturn(Boolean.FALSE);
        when(participantRepository.existsByTeamIdAndNumber(1L, 1)).thenReturn(Boolean.FALSE);
        SeasonStageException actualException = assertThrows(SeasonStageException.class,
                () -> participantService.createValidParticipantConfigurationForTeam(
                        1L, List.of(participantCreationData1))
        );
        assertEquals("In dieser Saisonphase kann keine Mannschaft mit Teilnehmern befüllt werden!",
                actualException.getMessage());
    }

    @Test
    void addParticipantToTeamOk() {

    }

    @Test
    void testAddParticipantToTeamParticipationEligibilityDoesNotExist() {

    }

    @Test
    void testAddParticipantToTeamParticipantForParticipationEligibilityAlreadyExists() {

    }

    @Test
    void testAddParticipantToTeamNumberNotAvailableForTeam() {

    }

    @Test
    void testAddParticipantToTeamWrongSeasonStage() {

    }

    @Test
    void testAddParticipantToTeamInvalidNumber() {

    }

    @Test
    void testDeleteParticipantOk() {

    }

    @Test
    void testDeleteParticipantThatDoesNotExist() {

    }

    @Test
    void testDeleteParticipantWrongSeasonStage() {

    }

    @Test
    void testDeleteParticipantWrongNumber() {

    }


}