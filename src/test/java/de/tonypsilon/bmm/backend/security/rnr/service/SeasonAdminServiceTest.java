package de.tonypsilon.bmm.backend.security.rnr.service;

import de.tonypsilon.bmm.backend.exception.AlreadyExistsException;
import de.tonypsilon.bmm.backend.exception.NotFoundException;
import de.tonypsilon.bmm.backend.season.service.SeasonService;
import de.tonypsilon.bmm.backend.security.rnr.data.SeasonAdmin;
import de.tonypsilon.bmm.backend.security.rnr.data.SeasonAdminData;
import de.tonypsilon.bmm.backend.security.rnr.data.SeasonAdminRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.security.provisioning.UserDetailsManager;

import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

class SeasonAdminServiceTest {

    private final SeasonAdminRepository seasonAdminRepository = mock(SeasonAdminRepository.class);
    private final SeasonService seasonService = mock(SeasonService.class);
    private final UserDetailsManager userDetailsManager = mock(UserDetailsManager.class);
    private SeasonAdminService seasonAdminService;
    private SeasonAdmin seasonAdmin1;
    private final SeasonAdminData seasonAdminData1 = new SeasonAdminData(1L, "user1");

    @BeforeEach
    private void setUp() {
        seasonAdminService = new SeasonAdminService(seasonAdminRepository,
                seasonService, userDetailsManager);
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
        assertEquals(seasonAdminData1, actual);
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
        assertEquals("Es gibt keinen Benutzer mit dem Namen user2!", actualException.getMessage());
    }

    @Test
    void testCreateSeasonAdminSeasonDoesNotExist() {
        when(userDetailsManager.userExists("user1")).thenReturn(Boolean.TRUE);
        when(seasonService.seasonExistsById(2L)).thenReturn(Boolean.FALSE);
        NotFoundException actualException = assertThrows(NotFoundException.class,
                () -> seasonAdminService.createSeasonAdmin(new SeasonAdminData(2L, "user1")));
        assertEquals("Es gibt keine Saison mit der ID 2!", actualException.getMessage());
    }

    @Test
    void testCreateSeasonAdminAlreadyExists() {
        when(userDetailsManager.userExists("user1")).thenReturn(Boolean.TRUE);
        when(seasonService.seasonExistsById(1L)).thenReturn(Boolean.TRUE);
        when(seasonAdminRepository.existsBySeasonIdAndUsername(1L, "user1")).thenReturn(Boolean.TRUE);
        AlreadyExistsException actualException = assertThrows(AlreadyExistsException.class,
                () -> seasonAdminService.createSeasonAdmin(new SeasonAdminData(1L, "user1")));
        assertEquals("Benutzer user1 ist bereits Administrator für die Saison mit ID 1!",
                actualException.getMessage());
    }

    @Test
    void testIsSeasonAdmin() {
        when(seasonAdminRepository.existsBySeasonIdAndUsername(1L, "user1")).thenReturn(Boolean.TRUE);
        when(seasonAdminRepository.existsBySeasonIdAndUsername(2L, "user1")).thenReturn(Boolean.FALSE);
        assertEquals(Boolean.TRUE, seasonAdminService.isSeasonAdmin(1L, "user1"));
        assertEquals(Boolean.FALSE, seasonAdminService.isSeasonAdmin(2L, "user1"));
    }

    @Test
    void testDeleteSeasonAdminOk() {
        when(seasonAdminRepository.findBySeasonIdAndUsername(1L, "user1")).thenReturn(Optional.of(seasonAdmin1));
        seasonAdminService.deleteSeasonAdmin(1L, "user1");
        verify(seasonAdminRepository, times(1)).delete(
                argThat(seasonAdmin -> seasonAdmin.getSeasonId().equals(1L)
                && seasonAdmin.getUsername().equals("user1"))
        );
    }

    @Test
    void testDeleteSeasonAdminThatDoesNotExist() {
        when(seasonAdminRepository.findBySeasonIdAndUsername(3L, "user1")).thenReturn(Optional.empty());
        NotFoundException actualException = assertThrows(NotFoundException.class,
                () -> seasonAdminService.deleteSeasonAdmin(3L, "user1"));
        assertEquals("Benutzer user1 ist kein Administrator für die Saison mit ID 3!", actualException.getMessage());
    }
}