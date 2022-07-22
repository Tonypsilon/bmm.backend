package de.tonypsilon.bmm.backend.security.rnr.service;

import de.tonypsilon.bmm.backend.exception.AlreadyExistsException;
import de.tonypsilon.bmm.backend.exception.NotFoundException;
import de.tonypsilon.bmm.backend.security.rnr.data.TeamAdmin;
import de.tonypsilon.bmm.backend.security.rnr.data.TeamAdminData;
import de.tonypsilon.bmm.backend.security.rnr.data.TeamAdminRepository;
import de.tonypsilon.bmm.backend.team.service.TeamService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.security.provisioning.UserDetailsManager;

import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

class TeamAdminServiceTest {

    private final TeamAdminRepository teamAdminRepository = mock(TeamAdminRepository.class);
    private final TeamService teamService = mock(TeamService.class);
    private final UserDetailsManager userDetailsManager = mock(UserDetailsManager.class);
    private TeamAdminService teamAdminService;
    private TeamAdmin teamAdmin1;
    private final TeamAdminData teamAdminData1 = new TeamAdminData(1L, "user1");

    @BeforeEach
    private void setUp() {
        teamAdminService = new TeamAdminService(teamAdminRepository, teamService, userDetailsManager);
        teamAdmin1 = new TeamAdmin();
        teamAdmin1.setTeamId(1L);
        teamAdmin1.setUsername("user1");
    }

    @Test
    void testCreateTeamAdminOk() {
        when(userDetailsManager.userExists("user1")).thenReturn(Boolean.TRUE);
        when(teamService.existsById(1L)).thenReturn(Boolean.TRUE);
        when(teamAdminRepository.existsByTeamIdAndUsername(1L, "user1")).thenReturn(Boolean.FALSE);
        when(teamAdminRepository.getByTeamIdAndUsername(1L, "user1")).thenReturn(teamAdmin1);

        TeamAdminData actual = teamAdminService.createTeamAdmin(new TeamAdminData(1L, "user1"));
        assertEquals(teamAdminData1, actual);
        verify(teamAdminRepository, times(1)).save(
                argThat(teamAdmin -> teamAdmin.getTeamId().equals(1L)
                && teamAdmin.getUsername().equals("user1"))
        );
    }

    @Test
    void testCreateTeamAdminUserDoesNotExist() {
        when(userDetailsManager.userExists("user2")).thenReturn(Boolean.FALSE);
        NotFoundException actualException = assertThrows(NotFoundException.class,
                () -> teamAdminService.createTeamAdmin(new TeamAdminData(1L, "user2")));
        assertEquals("Es gibt keinen Benutzer mit dem Namen user2!", actualException.getMessage());
    }

    @Test
    void testCreateTeamAdminTeamDoesNotExist() {
        when(userDetailsManager.userExists("user1")).thenReturn(Boolean.TRUE);
        when(teamService.existsById(2L)).thenReturn(Boolean.FALSE);
        NotFoundException actualException = assertThrows(NotFoundException.class,
                () -> teamAdminService.createTeamAdmin(new TeamAdminData(2L, "user1")));
        assertEquals("Es gibt kein Team mit der ID 2!", actualException.getMessage());
    }

    @Test
    void testCreateTeamAdminAlreadyExists() {
        when(userDetailsManager.userExists("user1")).thenReturn(Boolean.TRUE);
        when(teamService.existsById(1L)).thenReturn(Boolean.TRUE);
        when(teamAdminRepository.existsByTeamIdAndUsername(1L, "user1")).thenReturn(Boolean.TRUE);
        AlreadyExistsException actualException = assertThrows(AlreadyExistsException.class,
                () -> teamAdminService.createTeamAdmin(new TeamAdminData(1L, "user1")));
        assertEquals("Benutzer user1 ist bereits Administrator für das Team mit ID 1!",
                actualException.getMessage());
    }

    @Test
    void testIsTeamAdmin() {
        when(teamAdminRepository.existsByTeamIdAndUsername(1L, "user1")).thenReturn(Boolean.TRUE);
        when(teamAdminRepository.existsByTeamIdAndUsername(2L, "user2")).thenReturn(Boolean.FALSE);
        assertEquals(Boolean.TRUE, teamAdminService.isTeamAdmin(1L, "user1"));
        assertEquals(Boolean.FALSE, teamAdminService.isTeamAdmin(2L, "user2"));
    }

    @Test
    void testDeleteTeamAdminOk() {
        when(teamAdminRepository.findByTeamIdAndUsername(1L, "user1")).thenReturn(Optional.of(teamAdmin1));
        teamAdminService.deleteTeamAdmin(1L, "user1");
        verify(teamAdminRepository, times(1)).delete(
                argThat(teamAdmin -> teamAdmin.getTeamId().equals(1L)
                && teamAdmin.getUsername().equals("user1"))
        );
    }

    @Test
    void testDeleteTeamAdminThatDoesNotExist() {
        when(teamAdminRepository.findByTeamIdAndUsername(3L, "user3")).thenReturn(Optional.empty());
        NotFoundException actualException = assertThrows(NotFoundException.class,
                () -> teamAdminService.deleteTeamAdmin(3L, "user3"));
        assertEquals("Benutzer user3 ist kein Administrator für das Team mit ID 3!", actualException.getMessage());
    }

}