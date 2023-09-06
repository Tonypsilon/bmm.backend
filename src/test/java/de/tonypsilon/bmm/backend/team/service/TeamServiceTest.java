package de.tonypsilon.bmm.backend.team.service;

import de.tonypsilon.bmm.backend.exception.BadDataException;
import de.tonypsilon.bmm.backend.exception.NotFoundException;
import de.tonypsilon.bmm.backend.exception.SeasonStageException;
import de.tonypsilon.bmm.backend.organization.data.OrganizationData;
import de.tonypsilon.bmm.backend.organization.service.OrganizationService;
import de.tonypsilon.bmm.backend.season.service.SeasonService;
import de.tonypsilon.bmm.backend.season.service.SeasonStage;
import de.tonypsilon.bmm.backend.security.rnr.Role;
import de.tonypsilon.bmm.backend.security.rnr.Roles;
import de.tonypsilon.bmm.backend.security.rnr.data.UserData;
import de.tonypsilon.bmm.backend.security.rnr.service.UserService;
import de.tonypsilon.bmm.backend.team.data.Team;
import de.tonypsilon.bmm.backend.team.data.TeamCreationData;
import de.tonypsilon.bmm.backend.team.data.TeamData;
import de.tonypsilon.bmm.backend.team.data.TeamRepository;
import de.tonypsilon.bmm.backend.venue.service.VenueService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Optional;
import java.util.Set;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

class TeamServiceTest {

    private final TeamRepository teamRepository = mock(TeamRepository.class);
    private final SeasonService seasonService = mock(SeasonService.class);
    private final OrganizationService organizationService = mock(OrganizationService.class);
    private final VenueService venueService = mock(VenueService.class);
    private final UserService userService = mock(UserService.class);
    private TeamService teamService;
    private Team team1, team2;
    private final TeamData team2Data =
            new TeamData(2L, 1L, 2, 1L, "team2", "captain2");

    @BeforeEach
    void setUp() {
        teamService = new TeamService(
                teamRepository, seasonService, organizationService, venueService, userService);
        team1 = new Team();
        team1.setId(1L);
        team1.setOrganizationId(1L);
        team1.setNumber(1);
        team1.setVenueId(1L);
        team1.setName("team1");
        team1.setCaptainUsername("captain1");
        team2 = new Team();
        team2.setId(2L);
        team2.setOrganizationId(1L);
        team2.setNumber(2);
        team2.setVenueId(1L);
        team2.setName("team2");
        team2.setCaptainUsername("captain2");
    }

    @Test
    void testCreateTeamOk() {
        when(seasonService.seasonExistsById(1L)).thenReturn(Boolean.TRUE);
        when(seasonService.getStageOfSeason(1L)).thenReturn(SeasonStage.REGISTRATION);
        when(organizationService.getSeasonIdOfOrganization(1L)).thenReturn(1L);
        when(teamRepository.findByOrganizationId(1L)).thenReturn(Set.of(team1));
        when(teamRepository.getByOrganizationIdAndNumber(1L, 2)).thenReturn(team2);
        when(organizationService.getOrganizationById(1L)).thenReturn(new OrganizationData(1L, 1L, "org", Set.of(2L)));
        when(venueService.getClubIdByVenueId(1L)).thenReturn(2L);
        when(userService.getUserDataByUsername("captain2"))
                .thenReturn(new UserData("captain2", null, Set.of(Role.USER)));

        TeamData actual = teamService.createTeam(new TeamCreationData(
                1L, 2, 1L, "team2", "captain2"));
        assertThat(actual).isEqualTo(team2Data);
        verify(teamRepository, times(1)).save(
                argThat(team -> team.getOrganizationId().equals(1L)
                && team.getNumber().equals(2))
        );
        verify(userService).assignRoleToUser("captain2", Role.TEAM_ADMIN);
    }

    @Test
    void testCreateTeamOrganizationDoesNotExist() {
        when(organizationService.getSeasonIdOfOrganization(-1L))
                .thenThrow(new NotFoundException("Es gibt keine Organisation mit der ID -1!"));
        NotFoundException actualException = assertThrows(NotFoundException.class,
                () -> teamService.createTeam(new TeamCreationData(
                        -1L, 1, 1L, null, "captain")));
        assertThat(actualException.getMessage())
                .isEqualTo("Es gibt keine Organisation mit der ID -1!");
    }

    @Test
    void testCreateTeamSeasonNotInStageRegistration() {
        when(organizationService.getSeasonIdOfOrganization(1L)).thenReturn(1L);
        when(seasonService.getStageOfSeason(1L)).thenReturn(SeasonStage.RUNNING);
        SeasonStageException actualException = assertThrows(SeasonStageException.class,
                () -> teamService.createTeam(new TeamCreationData(
                        1L, 1, 1L, null, "captain")));
        assertThat(actualException.getMessage())
                .isEqualTo("Saison ist nicht in der Registrierungsphase!");
    }

    @Test
    void testCreateTeamWrongPlayingVenue() {
        when(organizationService.getSeasonIdOfOrganization(1L)).thenReturn(1L);
        when(seasonService.seasonExistsById(1L)).thenReturn(Boolean.TRUE);
        when(seasonService.getStageOfSeason(1L)).thenReturn(SeasonStage.REGISTRATION);
        when(teamRepository.findByOrganizationId(1L)).thenReturn(Set.of(team1));
        when(organizationService.getOrganizationById(1L)).thenReturn(new OrganizationData(1L, 1L, "org", Set.of(5L)));

        TeamCreationData creationData = new TeamCreationData(
                1L, 3, 1L, null, "captain");

        BadDataException actualException = assertThrows(BadDataException.class,
                () -> teamService.createTeam(creationData));

        assertThat(actualException).hasMessage("Das Spiellokal gehört zu keinem Verein der Organisation!");
    }

    @Test
    void testCreateTeamInvalidTeamNumber() {
        when(organizationService.getSeasonIdOfOrganization(1L)).thenReturn(1L);
        when(seasonService.seasonExistsById(1L)).thenReturn(Boolean.TRUE);
        when(seasonService.getStageOfSeason(1L)).thenReturn(SeasonStage.REGISTRATION);
        when(teamRepository.findByOrganizationId(1L)).thenReturn(Set.of(team1));
        when(organizationService.getOrganizationById(1L)).thenReturn(new OrganizationData(1L, 1L, "org", Set.of(2L)));
        when(venueService.getClubIdByVenueId(1L)).thenReturn(2L);

        BadDataException actualException = assertThrows(BadDataException.class,
                () -> teamService.createTeam(new TeamCreationData(
                        1L, 3, 1L, null, "captain")));
        assertThat(actualException.getMessage())
                .isEqualTo("Das neue Team hat nicht die passende Teamnummer. Erwartet: 2. Tatsächlich: 3.");
    }

    @Test
    void testDeleteTeamOk() {
        when(teamRepository.findById(2L)).thenReturn(Optional.of(team2));
        when(organizationService.getSeasonIdOfOrganization(1L)).thenReturn(1L);
        when(seasonService.getStageOfSeason(1L)).thenReturn(SeasonStage.REGISTRATION);
        when(teamRepository.findByOrganizationId(1L)).thenReturn(Set.of(team1, team2));

        teamService.deleteTeam(2L);
        verify(teamRepository, times(1)).delete(
                argThat(team -> team.getId().equals(2L)
                && team.getOrganizationId().equals(1L)
                && team.getNumber().equals(2))
        );
    }

    @Test
    void testDeleteTeamDoesNotExist() {
        when(teamRepository.findById(-1L)).thenReturn(Optional.empty());
        NotFoundException actualException = assertThrows(NotFoundException.class,
                () -> teamService.deleteTeam(-1L));
        assertThat(actualException.getMessage())
                .isEqualTo("Es gibt kein Team mit ID -1!");
    }

    @Test
    void testDeleteTeamSeasonNotInStageRegistration() {
        when(teamRepository.findById(2L)).thenReturn(Optional.of(team2));
        when(organizationService.getSeasonIdOfOrganization(1L)).thenReturn(1L);
        when(seasonService.getStageOfSeason(1L)).thenReturn(SeasonStage.PREPARATION);
        SeasonStageException actualException = assertThrows(SeasonStageException.class,
                () -> teamService.deleteTeam(2L));
        assertThat(actualException.getMessage())
                .isEqualTo("Saison ist nicht in der Registrierungsphase!");
    }

    @Test
    void testDeleteTeamNotHighestTeamNumber() {
        when(teamRepository.findById(1L)).thenReturn(Optional.of(team1));
        when(organizationService.getSeasonIdOfOrganization(1L)).thenReturn(1L);
        when(seasonService.getStageOfSeason(1L)).thenReturn(SeasonStage.REGISTRATION);
        when(teamRepository.findByOrganizationId(1L)).thenReturn(Set.of(team1, team2));

        BadDataException actualException = assertThrows(BadDataException.class,
                () -> teamService.deleteTeam(1L));
        assertThat(actualException.getMessage())
                .isEqualTo("Es kann nur das Team mit der höchsten Nummer gelöscht werden!");
    }

}