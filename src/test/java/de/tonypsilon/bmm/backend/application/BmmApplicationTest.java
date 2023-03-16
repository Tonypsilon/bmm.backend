package de.tonypsilon.bmm.backend.application;

import com.fasterxml.jackson.databind.ObjectMapper;
import de.tonypsilon.bmm.backend.club.data.ClubCreationData;
import de.tonypsilon.bmm.backend.club.data.ClubData;
import de.tonypsilon.bmm.backend.division.data.DivisionCreationData;
import de.tonypsilon.bmm.backend.division.data.DivisionData;
import de.tonypsilon.bmm.backend.organization.data.OrganizationCreationData;
import de.tonypsilon.bmm.backend.organization.data.OrganizationData;
import de.tonypsilon.bmm.backend.season.data.*;
import de.tonypsilon.bmm.backend.season.service.SeasonStage;
import de.tonypsilon.bmm.backend.security.rnr.Role;
import de.tonypsilon.bmm.backend.security.rnr.data.ClubAdminData;
import de.tonypsilon.bmm.backend.security.rnr.data.SeasonAdminData;
import de.tonypsilon.bmm.backend.security.rnr.data.UserData;
import de.tonypsilon.bmm.backend.team.data.TeamCreationData;
import de.tonypsilon.bmm.backend.team.data.TeamData;
import de.tonypsilon.bmm.backend.team.data.TeamDivisionLinkData;
import de.tonypsilon.bmm.backend.venue.data.VenueCreationData;
import de.tonypsilon.bmm.backend.venue.data.VenueData;
import io.restassured.RestAssured;
import io.restassured.response.Response;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.web.server.LocalServerPort;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.test.context.jdbc.Sql;
import org.springframework.test.jdbc.JdbcTestUtils;

import javax.sql.DataSource;

import java.util.*;

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
    @Sql(scripts = "classpath:test-user-data.sql",
            executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    @Sql(scripts = "classpath:clear-all-tables.sql",
            executionPhase = Sql.ExecutionPhase.AFTER_TEST_METHOD)
    void happyPathTest() throws Exception {
        baseUrl = "http://localhost:" + port;
        SeasonHelper seasonHelper = new SeasonHelper(baseUrl);
        UserHelper userHelper = new UserHelper(baseUrl);
        ClubHelper clubHelper = new ClubHelper(baseUrl);
        LoginHelper loginHelper = new LoginHelper(baseUrl);
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
                        Set.of(Role.SEASON_ADMIN)),
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

        // step 7a: create playing venues
        clubHelper.createVenuesForClubs(clubs.keySet());

        // step 7: Create 2 teams of each organization.


        // step 8: Move season to preparation stage.
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

        // step 9: Create a division for the season.
        DivisionData divisionData = RestAssured
            .given()
                .headers(seasonAdminHeaders)
                .body(objectMapper.writeValueAsString(new DivisionCreationData("the division", 1, 8, theSeason.id())))
            .when()
                .post(baseUrl + "/divisions")
            .then()
                .statusCode(HttpStatus.CREATED.value())
                .extract().response().as(DivisionData.class);
        assertThat(divisionData.name()).isEqualTo("the division");
        assertThat(divisionData.level()).isEqualTo(1);
        assertThat(divisionData.seasonId()).isEqualTo(theSeason.id());

        // step 10: Assign all 4 teams of the 2 organizations to the division.


        // TODO: Referees?

        // step 11: Move season to in progress stage. Verify that all matchdays are created properly.
    }

    private TeamData createTeam(TeamCreationData teamCreationData, HttpHeaders headers) throws Exception {
        return RestAssured
            .given()
                .headers(headers)
                .body(objectMapper.writeValueAsString(teamCreationData))
            .when()
                .post(baseUrl + "/teams")
            .then()
                .statusCode(HttpStatus.CREATED.value())
            .extract()
                .response()
                .as(TeamData.class);
    }
    
    private TeamDivisionLinkData createTeamDivisionLink(
            TeamDivisionLinkData teamDivisionLinkData, HttpHeaders headers)  throws Exception {
    	return RestAssured
    		.given()
    			.headers(headers)
    			.body(objectMapper.writeValueAsString(teamDivisionLinkData))
    		.when()
    		    .post(baseUrl + "/teamdivisionlinks")
    		.then()
    		    .statusCode(HttpStatus.CREATED.value())
    		.extract()
    		    .response()
    		    .as(TeamDivisionLinkData.class);
    }


}
