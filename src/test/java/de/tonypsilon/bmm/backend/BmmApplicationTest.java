package de.tonypsilon.bmm.backend;

import com.fasterxml.jackson.databind.ObjectMapper;
import de.tonypsilon.bmm.backend.club.data.ClubCreationData;
import de.tonypsilon.bmm.backend.club.data.ClubData;
import de.tonypsilon.bmm.backend.division.data.DivisionCreationData;
import de.tonypsilon.bmm.backend.division.data.DivisionData;
import de.tonypsilon.bmm.backend.organization.data.OrganizationCreationData;
import de.tonypsilon.bmm.backend.organization.data.OrganizationData;
import de.tonypsilon.bmm.backend.season.data.SeasonCreationData;
import de.tonypsilon.bmm.backend.season.data.SeasonData;
import de.tonypsilon.bmm.backend.season.data.SeasonStageChangeData;
import de.tonypsilon.bmm.backend.season.service.SeasonStage;
import de.tonypsilon.bmm.backend.security.rnr.Role;
import de.tonypsilon.bmm.backend.security.rnr.data.ClubAdminData;
import de.tonypsilon.bmm.backend.security.rnr.data.SeasonAdminData;
import de.tonypsilon.bmm.backend.security.rnr.data.UserData;
import de.tonypsilon.bmm.backend.team.data.TeamCreationData;
import de.tonypsilon.bmm.backend.team.data.TeamData;
import de.tonypsilon.bmm.backend.team.data.TeamDivisionAssignmentData;
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
import static org.junit.jupiter.api.Assertions.assertEquals;

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
        assertEquals(1, JdbcTestUtils.countRowsInTable(jdbcTemplate, "users"));
        assertEquals(1, JdbcTestUtils.countRowsInTable(jdbcTemplate, "authorities"));

        // step 1: log in and get cookies
        HttpHeaders headersAdmin = login(configuration.adminUsername(), configuration.adminPassword());

        // step 2: create season
        Response postSeasonResponse = RestAssured
            .given()
                .headers(headersAdmin)
                .body(objectMapper.writeValueAsString(new SeasonCreationData("test")))
            .when()
                .post(baseUrl + "/seasons")
            .then()
                .statusCode(HttpStatus.CREATED.value())
            .extract()
                .response();

        SeasonData theSeason = postSeasonResponse.as(SeasonData.class);
        assertThat(theSeason.name()).isEqualTo("test");
        assertThat(theSeason.stage()).isEqualTo(SeasonStage.REGISTRATION);

        // step 3: create a new user and make it season admin for the season
        Response postUserResponse = RestAssured
            .given()
                .headers(headersAdmin)
                .body(objectMapper.writeValueAsString(
                        new UserData(configuration.seasonAdminUsername(),
                                configuration.seasonAdminPassword(),
                                Set.of(Role.SEASON_ADMIN))))
            .when()
                .post(baseUrl + "/users")
            .then()
                .statusCode(HttpStatus.CREATED.value())
            .extract()
                .response();

        UserData seasonAdminUser = postUserResponse.as(UserData.class);
        assertThat(seasonAdminUser.username()).isEqualTo(configuration.seasonAdminUsername());
        assertThat(seasonAdminUser.password()).isNull();
        assertThat(seasonAdminUser.roles()).isEqualTo(Set.of(Role.SEASON_ADMIN));

        Response postSeasonAdminResponse = RestAssured
            .given()
                .headers(headersAdmin)
                .body(objectMapper.writeValueAsString(
                        new SeasonAdminData(theSeason.id(), seasonAdminUser.username())))
            .when()
                .post(baseUrl + "/seasonadmins")
            .then()
                .statusCode(HttpStatus.CREATED.value())
            .extract()
                .response();

        SeasonAdminData seasonAdminData = postSeasonAdminResponse.as(SeasonAdminData.class);
        assertThat(seasonAdminData.seasonId()).isEqualTo(theSeason.id());
        assertThat(seasonAdminData.username()).isEqualTo(seasonAdminUser.username());

        // step 4: Create 3 clubs and a club admin for each.
        ClubData clubOrga1 = createClub(new ClubCreationData("clubOrga1", 1, Boolean.TRUE), headersAdmin);
        assertThat(clubOrga1.name()).isEqualTo("clubOrga1");
        assertThat(clubOrga1.zps()).isEqualTo(1);
        assertThat(clubOrga1.active()).isTrue();

        ClubData clubOrga2 = createClub(new ClubCreationData("clubOrga2", 2, Boolean.TRUE), headersAdmin);
        assertThat(clubOrga2.name()).isEqualTo("clubOrga2");
        assertThat(clubOrga2.zps()).isEqualTo(2);
        assertThat(clubOrga2.active()).isTrue();

        ClubData clubSingle = createClub(new ClubCreationData("clubSingle1", 3, Boolean.TRUE), headersAdmin);
        assertThat(clubSingle.name()).isEqualTo("clubSingle1");
        assertThat(clubSingle.zps()).isEqualTo(3);
        assertThat(clubSingle.active()).isTrue();

        ClubAdminData clubAdminClubOrga1 = createClubAdminForClub(
                new ClubAdminData(clubOrga1.id(), "clubAdminClubOrga1"), headersAdmin);
        assertThat(clubAdminClubOrga1.clubId()).isEqualTo(clubOrga1.id());
        assertThat(clubAdminClubOrga1.username()).isEqualTo("clubAdminClubOrga1");

        ClubAdminData clubAdminClubOrga2 = createClubAdminForClub(
                new ClubAdminData(clubOrga2.id(), "clubAdminClubOrga2"), headersAdmin);
        assertThat(clubAdminClubOrga2.clubId()).isEqualTo(clubOrga2.id());
        assertThat(clubAdminClubOrga2.username()).isEqualTo("clubAdminClubOrga2");

        ClubAdminData clubAdminSingle = createClubAdminForClub(
                new ClubAdminData(clubSingle.id(), "clubAdminSingle"), headersAdmin);
        assertThat(clubAdminSingle.clubId()).isEqualTo(clubSingle.id());
        assertThat(clubAdminSingle.username()).isEqualTo("clubAdminSingle");

        // step 5: Create 2 organizations for the season. One with 2 clubs, one with a single one.
        // sub steps: log in as club admin of a respective clubs and fetch headers
        HttpHeaders headersClubAdminClubOrga1 = login("clubAdminClubOrga1", configuration.clubAdminPassword());
        OrganizationData organizationTwoClubs = createOrganization(
                new OrganizationCreationData(theSeason.id(),
                        "Organization Two Clubs",
                        Set.of(clubOrga1.id(), clubOrga2.id())),
                headersClubAdminClubOrga1);
        assertThat(organizationTwoClubs.seasonId()).isEqualTo(theSeason.id());
        assertThat(organizationTwoClubs.name()).isEqualTo("Organization Two Clubs");
        assertThat(organizationTwoClubs.clubIds()).containsExactlyInAnyOrder(clubOrga1.id(), clubOrga2.id());

        HttpHeaders headersClubAdminSingleClub = login("clubAdminSingle", configuration.clubAdminPassword());
        OrganizationData organizationSingleClub = createOrganization(
                new OrganizationCreationData(theSeason.id(),
                        clubSingle.name(),
                        Set.of(clubSingle.id())),
                headersClubAdminSingleClub);
        assertThat(organizationSingleClub.seasonId()).isEqualTo(theSeason.id());
        assertThat(organizationSingleClub.name()).isEqualTo(clubSingle.name());
        assertThat(organizationSingleClub.clubIds()).containsExactlyInAnyOrder(clubSingle.id());

        // step 6: Create 2 teams of each organization.
        TeamData organizationTwoClubsTeam1 = createTeam(
                new TeamCreationData(organizationTwoClubs.id(), 1), headersClubAdminClubOrga1);
        assertThat(organizationTwoClubsTeam1.organizationId()).isEqualTo(organizationTwoClubs.id());
        assertThat(organizationTwoClubsTeam1.number()).isEqualTo(1);

        TeamData organizationTwoClubsTeam2 = createTeam(
                new TeamCreationData(organizationTwoClubs.id(), 2), headersClubAdminClubOrga1);
        assertThat(organizationTwoClubsTeam2.organizationId()).isEqualTo(organizationTwoClubs.id());
        assertThat(organizationTwoClubsTeam2.number()).isEqualTo(2);

        TeamData organizationSingleTeam1 = createTeam(
                new TeamCreationData(organizationSingleClub.id(), 1), headersClubAdminSingleClub);
        assertThat(organizationSingleTeam1.organizationId()).isEqualTo(organizationSingleClub.id());
        assertThat(organizationSingleTeam1.number()).isEqualTo(1);

        TeamData organizationSingleTeam2 = createTeam(
                new TeamCreationData(organizationSingleClub.id(), 2), headersClubAdminSingleClub);
        assertThat(organizationSingleTeam2.organizationId()).isEqualTo(organizationSingleClub.id());
        assertThat(organizationSingleTeam2.number()).isEqualTo(2);

        // step 7: Move season to preparation stage.
        // substep: Log in as season admin and fetch headers
        HttpHeaders seasonAdminHeaders = login(seasonAdminUser.username(), configuration.seasonAdminPassword());
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

        // step 8: Create a division for the season.
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

        // step 9: Assign all 4 teams of the 2 organizations to the division.
        TeamDivisionAssignmentData team1Assignment = createTeamDivisionAssignment(
        		new TeamDivisionAssignmentData(organizationTwoClubsTeam1.id(), divisionData.id(), 1),
        		seasonAdminHeaders);
        assertThat(team1Assignment.teamId()).isEqualTo(organizationTwoClubsTeam1.id());
        assertThat(team1Assignment.divisionId()).isEqualTo(divisionData.id());
        assertThat(team1Assignment.number()).isEqualTo(1);
        
        TeamDivisionAssignmentData team2Assignment = createTeamDivisionAssignment(
        		new TeamDivisionAssignmentData(organizationTwoClubsTeam2.id(), divisionData.id(), 2),
        		seasonAdminHeaders);
        assertThat(team2Assignment.teamId()).isEqualTo(organizationTwoClubsTeam2.id());
        assertThat(team2Assignment.divisionId()).isEqualTo(divisionData.id());
        assertThat(team2Assignment.number()).isEqualTo(2);
        
        TeamDivisionAssignmentData team3Assignment = createTeamDivisionAssignment(
        		new TeamDivisionAssignmentData(organizationSingleTeam1.id(), divisionData.id(), 3),
        		seasonAdminHeaders);
        assertThat(team3Assignment.teamId()).isEqualTo(organizationSingleTeam1.id());
        assertThat(team3Assignment.divisionId()).isEqualTo(divisionData.id());
        assertThat(team3Assignment.number()).isEqualTo(3);
        
        TeamDivisionAssignmentData team4Assignment = createTeamDivisionAssignment(
        		new TeamDivisionAssignmentData(organizationSingleTeam2.id(), divisionData.id(), 4),
        		seasonAdminHeaders);
        assertThat(team4Assignment.teamId()).isEqualTo(organizationSingleTeam2.id());
        assertThat(team4Assignment.divisionId()).isEqualTo(divisionData.id());
        assertThat(team4Assignment.number()).isEqualTo(4);

        // step 10: Create 3 matchdays for the division and matches with the respective teams

        // TODO: Referees? Playing venues?

        // step 11: Move season to in progress stage.
    }

    private ClubData createClub(ClubCreationData clubCreationData, HttpHeaders headers) throws Exception {
        return RestAssured
            .given()
                .headers(headers)
                .body(objectMapper.writeValueAsString(clubCreationData))
            .when()
                .post(baseUrl + "/clubs")
            .then()
                .statusCode(HttpStatus.CREATED.value())
            .extract()
                .response()
                .as(ClubData.class);
    }

    private ClubAdminData createClubAdminForClub(ClubAdminData clubAdminCreationData, HttpHeaders headers)
            throws Exception{
        UserData userData = RestAssured
            .given()
                .headers(headers)
                .body(objectMapper.writeValueAsString(
                        new UserData(clubAdminCreationData.username(),
                                configuration.clubAdminPassword(),
                                Set.of(Role.CLUB_ADMIN))))
            .when()
                .post(baseUrl + "/users")
            .then()
                .statusCode(HttpStatus.CREATED.value())
            .extract()
                .response()
                .as(UserData.class);
        assertThat(userData.username()).isEqualTo(clubAdminCreationData.username());
        assertThat(userData.roles()).containsExactlyInAnyOrder(Role.CLUB_ADMIN);

        return RestAssured
            .given()
                .headers(headers)
                .body(objectMapper.writeValueAsString(clubAdminCreationData))
            .when()
                .post(baseUrl + "/clubadmins")
            .then()
                .statusCode(HttpStatus.CREATED.value())
            .extract()
                .response()
                .as(ClubAdminData.class);
    }

    private OrganizationData createOrganization(OrganizationCreationData organizationCreationData,
                                                HttpHeaders headers) throws Exception {
        return RestAssured
            .given()
                .headers(headers)
                .body(objectMapper.writeValueAsString(organizationCreationData))
            .when()
                .post(baseUrl + "/organizations")
            .then()
                .statusCode(HttpStatus.CREATED.value())
            .extract()
                .response()
                .as(OrganizationData.class);
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
    
    private TeamDivisionAssignmentData createTeamDivisionAssignment(
    		TeamDivisionAssignmentData teamDivisionAssignmentData, HttpHeaders headers)  throws Exception {
    	return RestAssured
    		.given()
    			.headers(headers)
    			.body(objectMapper.writeValueAsString(teamDivisionAssignmentData))
    		.when()
    		    .post(baseUrl + "/teamdivisionassignments")
    		.then()
    		    .statusCode(HttpStatus.CREATED.value())
    		.extract()
    		    .response()
    		    .as(TeamDivisionAssignmentData.class);
    }

    private HttpHeaders login(String username, String password) {
        Response loginResponse = RestAssured
                .given()
                .auth().preemptive().basic(username, password)
                .when()
                .get(baseUrl + "/user")
                .then()
                .statusCode(HttpStatus.OK.value())
                .extract().response();

        Map<String, String> loginCookies = createCookieMap(
                loginResponse.headers().getValues("Set-Cookie"));
        assertThat(loginCookies).containsKey("JSESSIONID")
                .containsKey("XSRF-TOKEN");

        HttpHeaders headers = new HttpHeaders();
        headers.setAccept(List.of(MediaType.APPLICATION_JSON));
        headers.setContentType(MediaType.APPLICATION_JSON);
        headers.add("Cookie", "JSESSIONID=" + loginCookies.get("JSESSIONID"));
        headers.add("Cookie", "XSRF-TOKEN=" + loginCookies.get("XSRF-TOKEN"));
        headers.add("X-XSRF-TOKEN", loginCookies.get("XSRF-TOKEN"));
        return headers;
    }

    private Map<String, String> createCookieMap(List<String> cookies) {
        Map<String, String> cookieMap = new HashMap<>();
        for (String cookie : cookies) {
            cookieMap.put(cookie.split("=")[0], cookie.split("=")[1].split(";")[0]);
        }
        return cookieMap;
    }
}
