package de.tonypsilon.bmm.backend.security.rnr.service;

import de.tonypsilon.bmm.backend.exception.AlreadyExistsException;
import de.tonypsilon.bmm.backend.exception.NotFoundException;
import de.tonypsilon.bmm.backend.season.service.SeasonService;
import de.tonypsilon.bmm.backend.security.rnr.Role;
import de.tonypsilon.bmm.backend.security.rnr.data.SeasonAdmin;
import de.tonypsilon.bmm.backend.security.rnr.data.SeasonAdminData;
import de.tonypsilon.bmm.backend.security.rnr.data.SeasonAdminRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.security.provisioning.UserDetailsManager;

import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

class SeasonAdminServiceTest {

    private final SeasonAdminRepository seasonAdminRepository = mock(SeasonAdminRepository.class);
    private final SeasonService seasonService = mock(SeasonService.class);
    private final UserDetailsManager userDetailsManager = mock(UserDetailsManager.class);
    private final UserService userService = mock(UserService.class);
    private SeasonAdminService seasonAdminService;
    private SeasonAdmin seasonAdmin1;
    private final SeasonAdminData seasonAdminData1 = new SeasonAdminData(1L, "user1");

    @BeforeEach
    void setUp() {
        seasonAdminService = new SeasonAdminService(seasonAdminRepository,
                seasonService,
                userDetailsManager,
                userService);
        seasonAdmin1 = new SeasonAdmin();
        seasonAdmin1.setSeasonId(1L);
        seasonAdmin1.setUsername("user1");
    }

    @Test
    void testCreateSeasonAdminOk() {
        when(userDetailsManager.userExists("user1")).thenReturn(Boolean.TRUE);
        when(seasonService.seasonExistsById(1L)).thenReturn(Boolean.TRUE);
        when(seasonAdminRepository.existsBySeasonIdAndUsername(1L, "user1")).thenReturn(Boolean.FALSE);
        when(seasonAdminRepository.getBySeasonIdAndUsername(1L, "user1")).thenReturn(seasonAdmin1);

        SeasonAdminData actual = seasonAdminService.createSeasonAdmin(new SeasonAdminData(1L, "user1"));
        assertThat(actual).isEqualTo(seasonAdminData1);
        verify(userService).assignRoleToUser("user1", Role.SEASON_ADMIN);
        verify(seasonAdminRepository, times(1)).save(
                argThat(seasonAdmin -> seasonAdmin.getSeasonId().equals(1L)
                && seasonAdmin.getUsername().equals("user1"))
        );
    }

    @Test
    void testCreateSeasonAdminUserDoesNotExist() {
        when(userDetailsManager.userExists("user2")).thenReturn(Boolean.FALSE);
        NotFoundException actualException = assertThrows(NotFoundException.class,
                () -> seasonAdminService.createSeasonAdmin(new SeasonAdminData(1L, "user2")));
        assertThat(actualException.getMessage())
            .isEqualTo("Es gibt keinen Benutzer mit dem Namen user2!");
    }

    @Test
    void testCreateSeasonAdminSeasonDoesNotExist() {
        when(userDetailsManager.userExists("user1")).thenReturn(Boolean.TRUE);
        when(seasonService.seasonExistsById(2L)).thenReturn(Boolean.FALSE);
        NotFoundException actualException = assertThrows(NotFoundException.class,
                () -> seasonAdminService.createSeasonAdmin(new SeasonAdminData(2L, "user1")));
        assertThat(actualException.getMessage())
            .isEqualTo("Es gibt keine Saison mit der ID 2!");
    }

    @Test
    void testCreateSeasonAdminAlreadyExists() {
        when(userDetailsManager.userExists("user1")).thenReturn(Boolean.TRUE);
        when(seasonService.seasonExistsById(1L)).thenReturn(Boolean.TRUE);
        when(seasonAdminRepository.existsBySeasonIdAndUsername(1L, "user1")).thenReturn(Boolean.TRUE);
        AlreadyExistsException actualException = assertThrows(AlreadyExistsException.class,
                () -> seasonAdminService.createSeasonAdmin(new SeasonAdminData(1L, "user1")));
        assertThat(actualException.getMessage())
            .isEqualTo("Benutzer user1 ist bereits Administrator für die Saison mit ID 1!");
    }

    @Test
    void testIsSeasonAdmin() {
        when(seasonAdminRepository.existsBySeasonIdAndUsername(1L, "user1")).thenReturn(Boolean.TRUE);
        when(seasonAdminRepository.existsBySeasonIdAndUsername(2L, "user1")).thenReturn(Boolean.FALSE);
        assertThat(seasonAdminService.isSeasonAdmin(1L, "user1")).isTrue();
        assertThat(seasonAdminService.isSeasonAdmin(2L, "user1")).isFalse();
    }

    @Test
    void testDeleteSeasonAdminOk() {
        when(seasonAdminRepository.findBySeasonIdAndUsername(1L, "user1")).thenReturn(Optional.of(seasonAdmin1));
        seasonAdminService.deleteSeasonAdmin(new SeasonAdminData(1L, "user1"));
        verify(seasonAdminRepository, times(1)).delete(
                argThat(seasonAdmin -> seasonAdmin.getSeasonId().equals(1L)
                && seasonAdmin.getUsername().equals("user1"))
        );
    }

    @Test
    void testDeleteSeasonAdminThatDoesNotExist() {
        when(seasonAdminRepository.findBySeasonIdAndUsername(3L, "user1")).thenReturn(Optional.empty());
        SeasonAdminData seasonAdminDeletionData = new SeasonAdminData(3L, "user1");
        NotFoundException actualException = assertThrows(NotFoundException.class,
                () -> seasonAdminService.deleteSeasonAdmin(seasonAdminDeletionData));
        assertThat(actualException.getMessage())
            .isEqualTo("Benutzer user1 ist kein Administrator für die Saison mit ID 3!");
    }
}