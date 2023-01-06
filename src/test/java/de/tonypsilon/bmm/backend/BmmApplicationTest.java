package de.tonypsilon.bmm.backend;

import com.fasterxml.jackson.databind.ObjectMapper;
import de.tonypsilon.bmm.backend.club.data.ClubCreationData;
import de.tonypsilon.bmm.backend.club.data.ClubData;
import de.tonypsilon.bmm.backend.season.data.CreateSeasonData;
import de.tonypsilon.bmm.backend.season.data.SeasonData;
import de.tonypsilon.bmm.backend.season.service.SeasonStage;
import de.tonypsilon.bmm.backend.security.rnr.Role;
import de.tonypsilon.bmm.backend.security.rnr.data.ClubAdminData;
import de.tonypsilon.bmm.backend.security.rnr.data.SeasonAdminData;
import de.tonypsilon.bmm.backend.security.rnr.data.UserData;
import io.restassured.RestAssured;
import io.restassured.response.ExtractableResponse;
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
public class BmmApplicationTest {

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
        ExtractableResponse<Response> loginResponse = RestAssured
            .given()
                .auth().preemptive().basic(configuration.adminUsername(), configuration.adminPassword())
            .when()
                .get(baseUrl + "/user")
            .then()
                .statusCode(HttpStatus.OK.value())
            .extract();

        Map<String, String> loginCookies = createCookieMap(loginResponse.response().headers().getValues("Set-Cookie"));
        assertThat(loginCookies).containsKey("JSESSIONID");
        assertThat(loginCookies).containsKey("XSRF-TOKEN");

        HttpHeaders headers = new HttpHeaders();
        headers.setAccept(List.of(MediaType.APPLICATION_JSON));
        headers.setContentType(MediaType.APPLICATION_JSON);
        headers.add("Cookie", "JSESSIONID=" + loginCookies.get("JSESSIONID"));
        headers.add("Cookie", "XSRF-TOKEN=" + loginCookies.get("XSRF-TOKEN"));
        headers.add("X-XSRF-TOKEN", loginCookies.get("XSRF-TOKEN"));

        // step 2: create season
        Response postSeasonResponse = RestAssured
            .given()
                .headers(headers)
                .body(objectMapper.writeValueAsString(new CreateSeasonData("test")))
            .when()
                .post(baseUrl + "/seasons")
            .then()
                .statusCode(HttpStatus.CREATED.value())
            .extract()
                .response();

        SeasonData createdSeason = postSeasonResponse.as(SeasonData.class);
        assertThat(createdSeason.name()).isEqualTo("test");
        assertThat(createdSeason.stage()).isEqualTo(SeasonStage.REGISTRATION);

        // step 3: create a new user and make it season admin for the season

        Response postUserResponse = RestAssured
            .given()
                .headers(headers)
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
                .headers(headers)
                .body(objectMapper.writeValueAsString(
                        new SeasonAdminData(createdSeason.id(), seasonAdminUser.username())))
            .when()
                .post(baseUrl + "/seasonadmins")
            .then()
                .statusCode(HttpStatus.CREATED.value())
            .extract()
                .response();

        SeasonAdminData seasonAdminData = postSeasonAdminResponse.as(SeasonAdminData.class);
        assertThat(seasonAdminData.seasonId()).isEqualTo(createdSeason.id());
        assertThat(seasonAdminData.username()).isEqualTo(seasonAdminUser.username());

        // step 4: Create 3 clubs and a club admin for each. Combine 2 of them to a single
        //         organization. The 3rd one will become an organization on its own.
        ClubData clubOrga1 = createClub(new ClubCreationData("clubOrga1", 1, Boolean.TRUE), headers);
        assertThat(clubOrga1.name()).isEqualTo("clubOrga1");
        assertThat(clubOrga1.zps()).isEqualTo(1);
        assertThat(clubOrga1.active()).isTrue();

        ClubData clubOrga2 = createClub(new ClubCreationData("clubOrga2", 2, Boolean.TRUE), headers);
        assertThat(clubOrga2.name()).isEqualTo("clubOrga2");
        assertThat(clubOrga2.zps()).isEqualTo(2);
        assertThat(clubOrga2.active()).isTrue();

        ClubData clubSingle = createClub(new ClubCreationData("clubSingle1", 3, Boolean.TRUE), headers);
        assertThat(clubSingle.name()).isEqualTo("clubSingle1");
        assertThat(clubSingle.zps()).isEqualTo(3);
        assertThat(clubSingle.active()).isTrue();

        ClubAdminData clubAdminClubOrga1 = createClubAdminForClub(
                new ClubAdminData(clubOrga1.id(), "clubAdminClubOrga1"), headers);
        assertThat(clubAdminClubOrga1.clubId()).isEqualTo(clubOrga1.id());
        assertThat(clubAdminClubOrga1.username()).isEqualTo("clubAdminClubOrga1");

        ClubAdminData clubAdminClubOrga2 = createClubAdminForClub(
                new ClubAdminData(clubOrga2.id(), "clubAdminClubOrga2"), headers);
        assertThat(clubAdminClubOrga2.clubId()).isEqualTo(clubOrga2.id());
        assertThat(clubAdminClubOrga2.username()).isEqualTo("clubAdminClubOrga2");

        ClubAdminData clubAdminSingle = createClubAdminForClub(
                new ClubAdminData(clubSingle.id(), "clubAdminSingle"), headers);
        assertThat(clubAdminSingle.clubId()).isEqualTo(clubSingle.id());
        assertThat(clubAdminSingle.username()).isEqualTo("clubAdminSingle");
    }

    private Map<String, String> createCookieMap(List<String> cookies) {
        Map<String, String> cookieMap = new HashMap<>();
        for (String cookie : cookies) {
            cookieMap.put(cookie.split("=")[0], cookie.split("=")[1].split(";")[0]);
        }
        return cookieMap;
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
}
