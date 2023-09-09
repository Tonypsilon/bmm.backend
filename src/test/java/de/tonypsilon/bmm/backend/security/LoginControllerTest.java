package de.tonypsilon.bmm.backend.security;

import com.fasterxml.jackson.databind.ObjectMapper;
import de.tonypsilon.bmm.backend.club.data.ClubData;
import de.tonypsilon.bmm.backend.datatypes.IdAndLabel;
import de.tonypsilon.bmm.backend.organization.data.OrganizationData;
import de.tonypsilon.bmm.backend.organization.service.OrganizationAdminService;
import de.tonypsilon.bmm.backend.season.data.SeasonData;
import de.tonypsilon.bmm.backend.season.service.SeasonService;
import de.tonypsilon.bmm.backend.season.service.SeasonStage;
import de.tonypsilon.bmm.backend.security.rnr.Roles;
import de.tonypsilon.bmm.backend.security.rnr.service.ClubAdminService;
import de.tonypsilon.bmm.backend.security.rnr.service.SeasonAdminService;
import de.tonypsilon.bmm.backend.security.rnr.service.TeamAdminService;
import de.tonypsilon.bmm.backend.team.data.TeamData;
import de.tonypsilon.bmm.backend.team.service.TeamService;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.web.servlet.MockMvc;

import java.util.List;
import java.util.Set;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@WebMvcTest(LoginController.class)
class LoginControllerTest {

    @Autowired
    private MockMvc mockMvc;

    @MockBean
    private SeasonAdminService seasonAdminService;

    @MockBean
    private ClubAdminService clubAdminService;

    @MockBean
    private TeamAdminService teamAdminService;

    @MockBean
    private TeamService teamService;

    @MockBean
    private OrganizationAdminService organizationAdminService;

    @MockBean
    private SeasonService seasonService;

    private final ObjectMapper objectMapper = new ObjectMapper();

    private static final String username = "username";

    @Test
    @WithMockUser(username = username, authorities = {Roles.ADMIN})
    void testGetUserAdmin() throws Exception {
        MockHttpServletResponse actualResponse = mockMvc
                .perform(get("/user"))
                .andExpect(status().isOk())
                .andReturn().getResponse();
        AuthenticationResponse actual = objectMapper.readValue(
                actualResponse.getContentAsString(),
                AuthenticationResponse.class);

        assertThat(actual.username()).isEqualTo(username);
        assertThat(actual.isAdmin()).isTrue();
        assertThat(actual.seasons()).isEmpty();
        assertThat(actual.clubs()).isEmpty();
        assertThat(actual.organizations()).isEmpty();
        assertThat(actual.teams()).isEmpty();
    }

    @Test
    @WithMockUser(username = username, authorities = {Roles.SEASON_ADMIN})
    void testGetUserSeasonAdmin() throws Exception {
        SeasonData season1 = new SeasonData(1L, "season1", SeasonStage.RUNNING);
        when(seasonAdminService.getSeasonsOfSeasonAdmin(username)).thenReturn(List.of(season1));

        MockHttpServletResponse actualResponse = mockMvc
                .perform(get("/user"))
                .andExpect(status().isOk())
                .andReturn().getResponse();
        AuthenticationResponse actual = objectMapper.readValue(
                actualResponse.getContentAsString(),
                AuthenticationResponse.class);

        assertThat(actual.username()).isEqualTo(username);
        assertThat(actual.isAdmin()).isFalse();
        assertThat(actual.seasons()).containsExactly(new IdAndLabel(season1.id(), season1.name()));
        assertThat(actual.clubs()).isEmpty();
        assertThat(actual.organizations()).isEmpty();
        assertThat(actual.teams()).isEmpty();
    }

    @Test
    @WithMockUser(username = username, authorities = {Roles.CLUB_ADMIN})
    void testGetUserClubAdmin() throws Exception {
        ClubData club1 = new ClubData(1L, "club1", 2, Boolean.TRUE);
        when(clubAdminService.getClubsOfClubAdmin(username)).thenReturn(List.of(club1));
        when(organizationAdminService.getOrganizationsOfUser(username)).thenReturn(Set.of());

        MockHttpServletResponse actualResponse = mockMvc
                .perform(get("/user"))
                .andExpect(status().isOk())
                .andReturn().getResponse();
        AuthenticationResponse actual = objectMapper.readValue(
                actualResponse.getContentAsString(),
                AuthenticationResponse.class);

        assertThat(actual.username()).isEqualTo(username);
        assertThat(actual.isAdmin()).isFalse();
        assertThat(actual.seasons()).isEmpty();
        assertThat(actual.clubs()).containsExactly(new IdAndLabel(club1.id(), club1.name()));
        assertThat(actual.organizations()).isEmpty();
        assertThat(actual.teams()).isEmpty();
    }

    @Test
    @WithMockUser(username = username, authorities = {Roles.CLUB_ADMIN})
    void testGetUserOrganizationAdmin() throws Exception {
        ClubData club1 = new ClubData(1L, "club1", 2, Boolean.TRUE);
        when(clubAdminService.getClubsOfClubAdmin(username)).thenReturn(List.of(club1));

        OrganizationData organization1 = new OrganizationData(2L, 3L, "theOrganization", Set.of(1L));
        when(organizationAdminService.getOrganizationsOfUser(username)).thenReturn(Set.of(organization1));
        when(seasonService.getStageOfSeason(3L)).thenReturn(SeasonStage.PREPARATION);
        when(seasonService.getSeasonById(3L)).thenReturn(new SeasonData(3L, "the season", SeasonStage.PREPARATION));

        MockHttpServletResponse actualResponse = mockMvc
                .perform(get("/user"))
                .andExpect(status().isOk())
                .andReturn().getResponse();
        AuthenticationResponse actual = objectMapper.readValue(
                actualResponse.getContentAsString(),
                AuthenticationResponse.class);

        assertThat(actual.username()).isEqualTo(username);
        assertThat(actual.isAdmin()).isFalse();
        assertThat(actual.seasons()).isEmpty();
        assertThat(actual.clubs()).containsExactly(new IdAndLabel(club1.id(), club1.name()));
        assertThat(actual.organizations()).containsExactly(
                new IdAndLabel(organization1.id(), organization1.name() + " - the season"));
        assertThat(actual.teams()).isEmpty();
    }

    @Test
    @WithMockUser(username = username, authorities = {Roles.TEAM_ADMIN})
    void testGetUserTeamAdmin() throws Exception {
        TeamData team1 = new TeamData(1L, 2L, 3, 4L, null, username);
        when(teamAdminService.getTeamsOfTeamAdmin(username)).thenReturn(List.of(team1));
        when(teamService.getSeasonIdByTeamId(1L)).thenReturn(5L);
        when(seasonService.getStageOfSeason(5L)).thenReturn(SeasonStage.RUNNING);
        when(teamService.getNameOfTeam(1L)).thenReturn("team 1");

        MockHttpServletResponse actualResponse = mockMvc
                .perform(get("/user"))
                .andExpect(status().isOk())
                .andReturn().getResponse();
        AuthenticationResponse actual = objectMapper.readValue(
                actualResponse.getContentAsString(),
                AuthenticationResponse.class);

        assertThat(actual.username()).isEqualTo(username);
        assertThat(actual.isAdmin()).isFalse();
        assertThat(actual.seasons()).isEmpty();
        assertThat(actual.clubs()).isEmpty();
        assertThat(actual.organizations()).isEmpty();
        assertThat(actual.teams()).containsExactly(new IdAndLabel(team1.id(), "team 1"));
    }
}