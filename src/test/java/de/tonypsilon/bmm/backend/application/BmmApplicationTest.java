package de.tonypsilon.bmm.backend.application;

import com.fasterxml.jackson.databind.ObjectMapper;
import de.tonypsilon.bmm.backend.club.data.ClubData;
import de.tonypsilon.bmm.backend.division.data.DivisionCreationData;
import de.tonypsilon.bmm.backend.division.data.DivisionData;
import de.tonypsilon.bmm.backend.organization.data.OrganizationData;
import de.tonypsilon.bmm.backend.participant.data.ParticipantData;
import de.tonypsilon.bmm.backend.participationeligibility.data.ParticipationEligibilityData;
import de.tonypsilon.bmm.backend.referee.data.RefereeCreationData;
import de.tonypsilon.bmm.backend.referee.facade.RefereeExternalData;
import de.tonypsilon.bmm.backend.season.data.*;
import de.tonypsilon.bmm.backend.season.service.SeasonStage;
import de.tonypsilon.bmm.backend.security.rnr.Role;
import de.tonypsilon.bmm.backend.security.rnr.data.ClubAdminData;
import de.tonypsilon.bmm.backend.security.rnr.data.SeasonAdminData;
import de.tonypsilon.bmm.backend.security.rnr.data.UserData;
import de.tonypsilon.bmm.backend.team.data.TeamData;
import de.tonypsilon.bmm.backend.team.data.TeamDivisionLinkData;
import de.tonypsilon.bmm.backend.venue.data.VenueData;
import io.restassured.RestAssured;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.web.server.LocalServerPort;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.test.context.jdbc.Sql;
import org.springframework.test.jdbc.JdbcTestUtils;

import javax.sql.DataSource;

import java.util.*;
import java.util.stream.Collectors;

import static org.assertj.core.api.Assertions.assertThat;

@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
class BmmApplicationTest {

    @LocalServerPort
    private Integer port;

    private final JdbcTemplate jdbcTemplate;

    private final ObjectMapper objectMapper = new ObjectMapper();

    private String baseUrl;

    @Autowired
    BmmApplicationTest(DataSource dataSource) {
        this.jdbcTemplate = new JdbcTemplate(dataSource);
    }

    @Autowired
    private BmmApplicationIntegrationTestConfiguration configuration;

    @Test
    @Sql(scripts = {"classpath:clear-all-tables.sql", "classpath:test-user-data.sql"},
            executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    void happyPathTest() throws Exception {
        baseUrl = "http://localhost:" + port;
        SeasonHelper seasonHelper = new SeasonHelper(baseUrl);
        UserHelper userHelper = new UserHelper(baseUrl);
        ClubHelper clubHelper = new ClubHelper(baseUrl);
        LoginHelper loginHelper = new LoginHelper(baseUrl);
        TeamHelper teamHelper = new TeamHelper(baseUrl);
        ParticipantHelper participantHelper = new ParticipantHelper(baseUrl);
        assertThat(JdbcTestUtils.countRowsInTable(jdbcTemplate, "users")).isEqualTo(1);
        assertThat(JdbcTestUtils.countRowsInTable(jdbcTemplate, "authorities")).isEqualTo(1);

        // step 1: log in and get cookies
        HttpHeaders headersAdmin = loginHelper.login(configuration.adminUsername(), configuration.adminPassword());

        // step 2: create season
        SeasonData theSeason = seasonHelper.createSeason(new SeasonCreationData("test"), headersAdmin);

        // step 3: create a new user and make it season admin for the season.
        UserData seasonAdminUser = userHelper.createUser(
                new UserData(configuration.seasonAdminUsername(),
                        configuration.seasonAdminPassword(),
                        Set.of(Role.SEASON_ADMIN),
                        "myEmail@mail.com",
                        null),
                headersAdmin);

        SeasonAdminData seasonAdminData = userHelper.createSeasonAdmin(
                new SeasonAdminData(theSeason.id(), seasonAdminUser.username()),
                headersAdmin);

        // step 4: Create 9 playing dates for the season.
        // substep: Log in as season admin and fetch headers
        HttpHeaders seasonAdminHeaders = loginHelper.login(seasonAdminUser.username(), configuration.seasonAdminPassword());
        Map<Integer, PlayingDateData> playingDates =
                seasonHelper.createNinePlayingDatesForSeason(theSeason.id(),seasonAdminHeaders);

        // step 5: Create clubs and a club admin for each.
        Map<ClubData, ClubAdminData> clubs = clubHelper.createClubs(headersAdmin);

        // step 6: Create organizations for the season.
        List<OrganizationData> organizations =
                clubHelper.createOrganizations(theSeason.id(),
                        clubs.keySet().stream().sorted(Comparator.comparing(ClubData::name)).toList());

        // step 7: create playing venues
        Map<ClubData, VenueData> venues = clubHelper.createVenuesForClubs(clubs.keySet());

        // step 8: Create participation eligibilities
        List<ParticipationEligibilityData> participationEligibilities =
                participantHelper.createParticipationEligibilities(theSeason.id(), organizations, seasonAdminHeaders);

        // step 9: Create 2 teams of each organization.
        Map<OrganizationData, List<TeamData>> teams = teamHelper.createTeams(organizations, venues);

        // step 10: Assign players to the teams
        Map<Long, List<ParticipationEligibilityData>> participationEligibilitiesByClub =
                participationEligibilities.stream()
                .collect(Collectors.groupingBy(ParticipationEligibilityData::clubId));
        Map<Long, List<ParticipationEligibilityData>> participationEligibilitiesByOrganization =
                new HashMap<>();
        for (List<ParticipationEligibilityData> participationEligibilityDataList :
                participationEligibilitiesByClub.values()) {
            participationEligibilitiesByOrganization.put(
                    getOrganizationIdByClubId(participationEligibilityDataList.get(0).clubId(),
                            organizations),
                    participationEligibilityDataList);
        }

        Set<ParticipantData> participants = new HashSet<>();
        for (OrganizationData organization : organizations) {
            participants.addAll(participantHelper.assignParticipantsToTeamsOfOrganization(organization,
                    participationEligibilitiesByOrganization.get(organization.id()).stream()
                            .map(ParticipationEligibilityData::id)
                            .toList(),
                    teams,
                    loginHelper.login(
                            clubs.keySet().stream()
                                    .filter(clubData -> clubData.id().equals(organization.clubIds().iterator().next()))
                                    .findFirst().orElseThrow()))
            );
        }
        assertThat(participants).hasSize(10*2*8); // 10 organizations, with 2 teams each, with 8 participants each

        // step 11: Move season to preparation stage.
        SeasonData theSeasonInPreparation = RestAssured
            .given()
                .headers(seasonAdminHeaders)
                .body(objectMapper.writeValueAsString(
                        new SeasonStageChangeData(theSeason.name(), SeasonStage.PREPARATION)))
            .when()
                .patch(baseUrl + "/seasons/" + theSeason.name())
            .then()
                .statusCode(HttpStatus.OK.value())
            .extract().response().as(SeasonData.class);
        assertThat(theSeasonInPreparation.id()).isEqualTo(theSeason.id());
        assertThat(theSeasonInPreparation.name()).isEqualTo(theSeason.name());
        assertThat(theSeasonInPreparation.stage()).isEqualTo(SeasonStage.PREPARATION);

        // step 12: Create two divisions for the season.
        DivisionData firstDivision = seasonHelper.createDivision(
                new DivisionCreationData("First Division", 1, 8, theSeasonInPreparation.id(), 10), seasonAdminHeaders);
        DivisionData secondDivision = seasonHelper.createDivision(
                new DivisionCreationData("Second Division", 2, 8, theSeasonInPreparation.id(), 10),seasonAdminHeaders);


        // step 13: Assign all teams of the organizations to the divisions.
        Map<DivisionData, List<TeamDivisionLinkData>> teamsToDivisions =
                teamHelper.linkTeamsToDivisions(teams, firstDivision, secondDivision, seasonAdminHeaders);

        // step 14: Create a referee.
        RefereeExternalData referee = RestAssured
                .given()
                .headers(seasonAdminHeaders)
                .body(objectMapper.writeValueAsString(new RefereeCreationData(
                        theSeasonInPreparation.id(), "Mister", "Referee", "mister@referee.com")))
                .when()
                .post(baseUrl + "/referees")
                .then()
                .statusCode(HttpStatus.CREATED.value())
                .extract()
                .response()
                .as(RefereeExternalData.class);
        assertThat(referee.seasonId()).isEqualTo(theSeasonInPreparation.id());
        assertThat(referee.forename()).isEqualTo("Mister");
        assertThat(referee.surname()).isEqualTo("Referee");

        // step 15: Move season to running stage. Verify that all matchdays are created properly.
        SeasonData theSeasonRunning = RestAssured
                .given()
                .headers(seasonAdminHeaders)
                .body(objectMapper.writeValueAsString(
                        new SeasonStageChangeData(theSeason.name(), SeasonStage.RUNNING)))
                .when()
                .patch(baseUrl + "/seasons/" + theSeason.name())
                .then()
                .statusCode(HttpStatus.OK.value())
                .extract().response().as(SeasonData.class);
        assertThat(theSeasonRunning.id()).isEqualTo(theSeason.id());
        assertThat(theSeasonRunning.name()).isEqualTo(theSeason.name());
        assertThat(theSeasonRunning.stage()).isEqualTo(SeasonStage.RUNNING);

        assertThat(JdbcTestUtils.countRowsInTable(jdbcTemplate, "match_"))
                .isEqualTo(2*5*9); // 2 divisions, 5 matches per matchday, 9 matchdays

        // step 16: Enter results for one specific match of the first matchday.
        // Start by entering 8 games consisting of no results yet.
        // Delete game of board 4
        // Add a new game of board 4 with different players.
        // Modify all the games: Enter results.
        // Then, finally, close the match. Verify that there are 8 games in the database.
    }

    private Long getOrganizationIdByClubId(Long clubId, Collection<OrganizationData> organizations) {
        return organizations.stream()
                .filter(organizationData -> organizationData.clubIds().contains(clubId))
                .findFirst()
                .orElseThrow()
                .id();
    }

}
