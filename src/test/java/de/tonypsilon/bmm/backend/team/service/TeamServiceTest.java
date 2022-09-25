package de.tonypsilon.bmm.backend.team.service;

import de.tonypsilon.bmm.backend.exception.BadDataException;
import de.tonypsilon.bmm.backend.exception.NotFoundException;
import de.tonypsilon.bmm.backend.exception.SeasonStageException;
import de.tonypsilon.bmm.backend.organization.service.OrganizationService;
import de.tonypsilon.bmm.backend.season.service.SeasonService;
import de.tonypsilon.bmm.backend.season.service.SeasonStage;
import de.tonypsilon.bmm.backend.team.data.Team;
import de.tonypsilon.bmm.backend.team.data.TeamCreationData;
import de.tonypsilon.bmm.backend.team.data.TeamData;
import de.tonypsilon.bmm.backend.team.data.TeamRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

class TeamServiceTest {

    private final TeamRepository teamRepository = mock(TeamRepository.class);
    private final SeasonService seasonService = mock(SeasonService.class);
    private final OrganizationService organizationService = mock(OrganizationService.class);
    private TeamService teamService;
    private Team team1, team2;
    private final TeamData team2Data = new TeamData(2L, 1L, 1L, 2);

    @BeforeEach
    private void setUp() {
        teamService = new TeamService(teamRepository, seasonService, organizationService);
        team1 = new Team();
        team1.setId(1L);
        team1.setSeasonId(1L);
        team1.setOrganizationId(1L);
        team1.setNumber(1);
        team2 = new Team();
        team2.setId(2L);
        team2.setSeasonId(1L);
        team2.setOrganizationId(1L);
        team2.setNumber(2);
    }

    @Test
    void testCreateTeamOk() {
        when(seasonService.seasonExistsById(1L)).thenReturn(Boolean.TRUE);
        when(seasonService.getStageOfSeason(1L)).thenReturn(SeasonStage.REGISTRATION);
        when(organizationService.existsByIdAndSeasonId(1L, 1L)).thenReturn(Boolean.TRUE);
        when(teamRepository.findBySeasonIdAndOrganizationId(1L, 1L)).thenReturn(List.of(team1));
        when(teamRepository.getBySeasonIdAndOrganizationIdAndNumber(1L, 1L, 2)).thenReturn(team2);

        TeamData actual = teamService.createTeam(new TeamCreationData(1L, 1L, 2));
        assertEquals(actual, team2Data);
        verify(teamRepository, times(1)).save(
                argThat(team -> team.getSeasonId().equals(1L)
                &&  team.getOrganizationId().equals(1L)
                && team.getNumber().equals(2))
        );
    }

    @Test
    void testCreateTeamSeasonDoesNotExist() {
        when(seasonService.seasonExistsById(-1L)).thenReturn(Boolean.FALSE);
        NotFoundException actualException = assertThrows(NotFoundException.class,
                () -> teamService.createTeam(new TeamCreationData(-1L, 1L, 1)));
        assertEquals("Es gibt keine Saison mit ID -1!", actualException.getMessage());
    }

    @Test
    void testCreateTeamSeasonNotInStageRegistration() {
        when(seasonService.seasonExistsById(1L)).thenReturn(Boolean.TRUE);
        when(seasonService.getStageOfSeason(1L)).thenReturn(SeasonStage.RUNNING);
        SeasonStageException actualException = assertThrows(SeasonStageException.class,
                () -> teamService.createTeam(new TeamCreationData(1L, 1L, 1)));
        assertEquals("Saison ist nicht in der Registrierungsphase!", actualException.getMessage());
    }

    @Test
    void testCreateTeamClubDoesNotExist() {
        when(seasonService.seasonExistsById(1L)).thenReturn(Boolean.TRUE);
        when(seasonService.getStageOfSeason(1L)).thenReturn(SeasonStage.REGISTRATION);
        when(organizationService.existsByIdAndSeasonId(-1L, 1L)).thenReturn(Boolean.FALSE);
        NotFoundException actualException = assertThrows(NotFoundException.class,
                () -> teamService.createTeam(new TeamCreationData(1L, -1L, 1)));
        assertEquals("Es gibt keine Organisation mit ID -1 für die Saison mit der ID 1!", actualException.getMessage());
    }

    @Test
    void testCreateTeamInvalidTeamNumber() {
        when(seasonService.seasonExistsById(1L)).thenReturn(Boolean.TRUE);
        when(seasonService.getStageOfSeason(1L)).thenReturn(SeasonStage.REGISTRATION);
        when(organizationService.existsByIdAndSeasonId(1L, 1L)).thenReturn(Boolean.TRUE);
        when(teamRepository.findBySeasonIdAndOrganizationId(1L, 1L)).thenReturn(List.of(team1));

        BadDataException actualException = assertThrows(BadDataException.class,
                () -> teamService.createTeam(new TeamCreationData(1L, 1L, 3)));
        assertEquals("Das neue Team hat nicht die passende Teamnummer. Erwartet: 2. Tatsächlich: 3.",
                actualException.getMessage());
    }

    @Test
    void testDeleteTeamOk() {
        when(teamRepository.findById(2L)).thenReturn(Optional.of(team2));
        when(seasonService.getStageOfSeason(1L)).thenReturn(SeasonStage.REGISTRATION);
        when(teamRepository.findBySeasonIdAndOrganizationId(1L, 1L)).thenReturn(List.of(team1, team2));

        teamService.deleteTeam(2L);
        verify(teamRepository, times(1)).delete(
                argThat(team -> team.getId().equals(2L)
                && team.getSeasonId().equals(1L)
                && team.getOrganizationId().equals(1L)
                && team.getNumber().equals(2))
        );
    }

    @Test
    void testDeleteTeamDoesNotExist() {
        when(teamRepository.findById(-1L)).thenReturn(Optional.empty());
        NotFoundException actualException = assertThrows(NotFoundException.class,
                () -> teamService.deleteTeam(-1L));
        assertEquals("Es gibt kein Team mit ID -1!", actualException.getMessage());
    }

    @Test
    void testDeleteTeamSeasonNotInStageRegistration() {
        when(teamRepository.findById(2L)).thenReturn(Optional.of(team2));
        when(seasonService.getStageOfSeason(1L)).thenReturn(SeasonStage.PREPARATION);
        SeasonStageException actualException = assertThrows(SeasonStageException.class,
                () -> teamService.deleteTeam(2L));
        assertEquals("Saison ist nicht in der Registrierungsphase!", actualException.getMessage());
    }

    @Test
    void testDeleteTeamNotHighestTeamNumber() {
        when(teamRepository.findById(1L)).thenReturn(Optional.of(team1));
        when(seasonService.getStageOfSeason(1L)).thenReturn(SeasonStage.REGISTRATION);
        when(teamRepository.findBySeasonIdAndOrganizationId(1L, 1L)).thenReturn(List.of(team1, team2));

        BadDataException actualException = assertThrows(BadDataException.class,
                () -> teamService.deleteTeam(1L));
        assertEquals("Es kann nur das Team mit der höchsten Nummer gelöscht werden!",
                actualException.getMessage());
    }

}