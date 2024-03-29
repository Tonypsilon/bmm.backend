package de.tonypsilon.bmm.backend.security.rnr.service;

import de.tonypsilon.bmm.backend.club.service.ClubService;
import de.tonypsilon.bmm.backend.exception.AlreadyExistsException;
import de.tonypsilon.bmm.backend.exception.NotFoundException;
import de.tonypsilon.bmm.backend.security.rnr.Role;
import de.tonypsilon.bmm.backend.security.rnr.data.ClubAdmin;
import de.tonypsilon.bmm.backend.security.rnr.data.ClubAdminData;
import de.tonypsilon.bmm.backend.security.rnr.data.ClubAdminRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.security.provisioning.UserDetailsManager;

import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.*;

class ClubAdminServiceTest {

    private final ClubAdminRepository clubAdminRepository = mock(ClubAdminRepository.class);
    private final ClubService clubService = mock(ClubService.class);
    private final UserDetailsManager userDetailsManager = mock(UserDetailsManager.class);
    private final UserService userService = mock(UserService.class);
    private ClubAdminService clubAdminService;
    private ClubAdmin clubAdmin1;
    private final ClubAdminData clubAdminData1 = new ClubAdminData(1L, "user1");

    @BeforeEach
    void setUp() {
        clubAdminService = new ClubAdminService(clubAdminRepository, clubService, userDetailsManager, userService);
        clubAdmin1 = new ClubAdmin();
        clubAdmin1.setClubId(1L);
        clubAdmin1.setUsername("user1");
    }

    @Test
    void testCreateClubAdminOk() {
        when(userDetailsManager.userExists("user1")).thenReturn(Boolean.TRUE);
        when(clubService.clubExistsById(1L)).thenReturn(Boolean.TRUE);
        when(clubAdminRepository.existsByClubIdAndUsername(1L, "user1")).thenReturn(Boolean.FALSE);
        when(clubAdminRepository.getByClubIdAndUsername(1L, "user1")).thenReturn(clubAdmin1);

        ClubAdminData actual = clubAdminService.createClubAdmin(new ClubAdminData(1L, "user1"));
        assertThat(actual).isEqualTo(clubAdminData1);
        verify(userService).assignRoleToUser("user1", Role.CLUB_ADMIN);
        verify(clubAdminRepository, times(1)).save(
                argThat(clubAdmin -> clubAdmin.getClubId().equals(1L)
                && clubAdmin.getUsername().equals("user1"))
        );
    }

    @Test
    void testCreateClubAdminUserDoesNotExist() {
        when(userDetailsManager.userExists("user2")).thenReturn(Boolean.FALSE);
        NotFoundException actualException = assertThrows(NotFoundException.class,
                () -> clubAdminService.createClubAdmin(new ClubAdminData(1L, "user2")));
        assertThat(actualException.getMessage())
            .isEqualTo("Es gibt keinen Benutzer mit dem Namen user2!");
    }

    @Test
    void testCreateClubAdminClubDoesNotExist() {
        when(userDetailsManager.userExists("user1")).thenReturn(Boolean.TRUE);
        when(clubService.clubExistsById(2L)).thenReturn(Boolean.FALSE);
        NotFoundException actualException = assertThrows(NotFoundException.class,
                () -> clubAdminService.createClubAdmin(new ClubAdminData(2L, "user1")));
        assertThat(actualException.getMessage())
            .isEqualTo("Es gibt keinen Verein mit der ID 2!");
    }

    @Test
    void testCreateClubAdminAlreadyExists() {
        when(userDetailsManager.userExists("user1")).thenReturn(Boolean.TRUE);
        when(clubService.clubExistsById(1L)).thenReturn(Boolean.TRUE);
        when(clubAdminRepository.existsByClubIdAndUsername(1L, "user1")).thenReturn(Boolean.TRUE);
        AlreadyExistsException actualException = assertThrows(AlreadyExistsException.class,
                () -> clubAdminService.createClubAdmin(new ClubAdminData(1L, "user1")));
        assertThat(actualException.getMessage())
            .isEqualTo("Benutzer user1 ist bereits Administrator für den Verein mit ID 1!");
    }

    @Test
    void testIsClubAdmin() {
        when(clubAdminRepository.existsByClubIdAndUsername(1L, "user1")).thenReturn(Boolean.TRUE);
        when(clubAdminRepository.existsByClubIdAndUsername(2L, "user3")).thenReturn(Boolean.FALSE);
        assertThat(clubAdminService.isClubAdmin(1L, "user1")).isTrue();
        assertThat(clubAdminService.isClubAdmin(2L, "user3")).isFalse();
    }

    @Test
    void testDeleteClubAdminOk() {
        when(clubAdminRepository.findByClubIdAndUsername(1L, "user1")).thenReturn(Optional.of(clubAdmin1));
        clubAdminService.deleteClubAdmin(new ClubAdminData(1L, "user1"));
        verify(clubAdminRepository, times(1)).delete(
                argThat(clubAdmin -> clubAdmin.getClubId().equals(1L)
                && clubAdmin.getUsername().equals("user1"))
        );
    }

    @Test
    void testDeleteClubAdminThatDoesNotExist() {
        when(clubAdminRepository.findByClubIdAndUsername(1L, "user3")).thenReturn(Optional.empty());
        ClubAdminData clubAdminDeletionData = new ClubAdminData(1L, "user3");
        NotFoundException actualException = assertThrows(NotFoundException.class,
                () -> clubAdminService.deleteClubAdmin(clubAdminDeletionData));
        assertThat(actualException.getMessage())
            .isEqualTo("Benutzer user3 ist kein Administrator für den Verein mit ID 1!");
    }

}