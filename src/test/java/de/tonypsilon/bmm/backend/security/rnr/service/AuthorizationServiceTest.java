package de.tonypsilon.bmm.backend.security.rnr.service;

import de.tonypsilon.bmm.backend.organization.service.OrganizationService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.springframework.security.access.AccessDeniedException;

import java.util.Set;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

class AuthorizationServiceTest {

    private final ClubAdminService clubAdminService = mock(ClubAdminService.class);
    private final OrganizationService organizationService = mock(OrganizationService.class);
    private final SeasonAdminService seasonAdminService = mock(SeasonAdminService.class);
    private AuthorizationService authorizationService;

    @BeforeEach
    void setUp() {
        this.authorizationService = new AuthorizationService(clubAdminService,
                organizationService,
                seasonAdminService);
    }

    @Test
    void testVerifyUserIsClubAdminOfAnyClubSuccess() {
        String username = "username";
        Set<Long> clubIds = Set.of(1L, 2L);
        when(clubAdminService.getAdminsOfClub(1L)).thenReturn(Set.of("some user", "another user"));
        when(clubAdminService.getAdminsOfClub(2L)).thenReturn(Set.of("username", "some user"));

        ArgumentCaptor<Long> clubIdArgumentCaptor = ArgumentCaptor.forClass(Long.class);
        authorizationService.verifyUserIsClubAdminOfAnyClub(username, clubIds);
        verify(clubAdminService, times(2)).getAdminsOfClub(clubIdArgumentCaptor.capture());
        assertThat(clubIdArgumentCaptor.getAllValues()).containsExactlyInAnyOrder(1L, 2L);
    }

    @Test
    void testVerifyUserIsClubAdminOfAnyClubFailure() {
        String username = "username";
        Set<Long> clubIds = Set.of(1L, 2L);
        when(clubAdminService.getAdminsOfClub(1L)).thenReturn(Set.of("some user", "another user"));
        when(clubAdminService.getAdminsOfClub(2L)).thenReturn(Set.of("username2", "some user"));

        ArgumentCaptor<Long> clubIdArgumentCaptor = ArgumentCaptor.forClass(Long.class);

        AccessDeniedException actualException = assertThrows(AccessDeniedException.class,
                () -> authorizationService.verifyUserIsClubAdminOfAnyClub(username, clubIds));
        verify(clubAdminService, times(2)).getAdminsOfClub(clubIdArgumentCaptor.capture());
        assertThat(clubIdArgumentCaptor.getAllValues()).containsExactlyInAnyOrder(1L, 2L);

        assertThat(actualException).hasMessage("username hat nicht ausreichend Vereinsadministrationsrechte!");
    }

    @Test
    void testVerifyUserIsClubAdminOfOrganizationSuccess() {

    }

    @Test
    void testVerifyUserIsClubAAdminOfOrganizationFailure() {

    }

    @Test
    void testVerifyUserIsSeasonOfSeasonAdminSuccess() {
        String username = "username";
        Long seasonId = 1L;
        when(seasonAdminService.isSeasonAdmin(seasonId, username)).thenReturn(Boolean.TRUE);
        authorizationService.verifyUserIsSeasonAdminOfSeason(username, seasonId);
        verify(seasonAdminService).isSeasonAdmin(seasonId, username);
        verifyNoMoreInteractions(seasonAdminService);
    }

    @Test
    void testVerifyUserIsSeasonAdminOfSeasonFailure() {
        String username = "username";
        Long seasonId = 1L;
        when(seasonAdminService.isSeasonAdmin(seasonId, username)).thenReturn(Boolean.FALSE);

        AccessDeniedException actualException = assertThrows(AccessDeniedException.class,
                () -> authorizationService.verifyUserIsSeasonAdminOfSeason(username, seasonId));
        verify(seasonAdminService).isSeasonAdmin(seasonId, username);
        assertThat(actualException).hasMessage("username hat nicht ausreichend Saisonadministrationsrechte!");
    }
}