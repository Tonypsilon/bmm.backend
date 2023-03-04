package de.tonypsilon.bmm.backend.teamcaptain.service;

import de.tonypsilon.bmm.backend.exception.AlreadyExistsException;
import de.tonypsilon.bmm.backend.exception.NotFoundException;
import de.tonypsilon.bmm.backend.team.service.TeamService;
import de.tonypsilon.bmm.backend.teamcaptain.data.*;
import de.tonypsilon.bmm.backend.validation.service.ValidationService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class TeamCaptainServiceTest {

    private final TeamCaptainRepository teamCaptainRepository = mock(TeamCaptainRepository.class);
    private final TeamService teamService = mock(TeamService.class);
    private final ValidationService validationService = new ValidationService();
    private TeamCaptainService teamCaptainService;
    private final TeamCaptainData teamCaptainData1 = new TeamCaptainData(
            1L, 2L, "team@admin.com", "01234567", "Team", "Admin");
    private TeamCaptain teamCaptain1;

    @BeforeEach
    void setUp() {
        this.teamCaptainService = new TeamCaptainService(teamCaptainRepository,
                teamService,
                validationService);
        teamCaptain1 = new TeamCaptain();
        teamCaptain1.setId(1L);
        teamCaptain1.setTeamId(2L);
        teamCaptain1.setEmailAddress("team@admin.com");
        teamCaptain1.setPhoneNumber("01234567");
        teamCaptain1.setForename("Team");
        teamCaptain1.setSurname("Admin");
    }

    @Test
    void testCreateTeamCaptainOk() {
        CreateTeamCaptainData createTeamCaptainData = new CreateTeamCaptainData(
                2L, "team@admin.com", "01234567", "Team", "Admin");
        when(teamService.existsById(2L)).thenReturn(Boolean.TRUE);
        when(teamCaptainRepository.existsByTeamId(2L)).thenReturn(Boolean.FALSE);
        when(teamCaptainRepository.getByTeamId(2L)).thenReturn(teamCaptain1);
        TeamCaptainData actual = teamCaptainService.createTeamCaptain(createTeamCaptainData);
        assertThat(actual).isEqualTo(teamCaptainData1);
    }

    @Test
    void testCreateTeamCaptainTeamDoesNotExist() {
        CreateTeamCaptainData createTeamCaptainData = new CreateTeamCaptainData(
                1L, "team@admin.com", "01234567", "Team", "Admin");
        when(teamService.existsById(1L)).thenReturn(Boolean.FALSE);
        NotFoundException actualException = assertThrows(NotFoundException.class,
                () -> teamCaptainService.createTeamCaptain(createTeamCaptainData));
        assertThat(actualException.getMessage())
                .isEqualTo("Es gibt keine Mannschaft mit der ID 1!");
    }

    @Test
    void testCreateTeamCaptainTeamAlreadyHasCaptain() {
        CreateTeamCaptainData createTeamCaptainData = new CreateTeamCaptainData(
                2L, "team@admin.com", "01234567", "Team", "Admin");
        when(teamService.existsById(2L)).thenReturn(Boolean.TRUE);
        when(teamCaptainRepository.existsByTeamId(2L)).thenReturn(Boolean.TRUE);
        AlreadyExistsException actualException = assertThrows(AlreadyExistsException.class,
                () -> teamCaptainService.createTeamCaptain(createTeamCaptainData));
        assertThat(actualException.getMessage())
                .isEqualTo("Es gibt bereits einen Mannschaftsleiter für die Mannschaft mit der ID 2!");
    }

}