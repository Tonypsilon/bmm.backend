package de.tonypsilon.bmm.backend;

import com.fasterxml.jackson.databind.ObjectMapper;
import de.tonypsilon.bmm.backend.season.data.SeasonCreationData;
import de.tonypsilon.bmm.backend.season.data.SeasonData;
import de.tonypsilon.bmm.backend.season.service.SeasonStage;
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

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;

@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
public class BmmApplicationTest {

    @LocalServerPort
    private Integer port;

    private final JdbcTemplate jdbcTemplate;

    private final ObjectMapper objectMapper = new ObjectMapper();

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
        final String baseUrl = "http://localhost:" + port;
        assertEquals(1, JdbcTestUtils.countRowsInTable(jdbcTemplate, "users"));
        assertEquals(1, JdbcTestUtils.countRowsInTable(jdbcTemplate, "authorities"));

        // step 1: log in and get cookies
        ExtractableResponse<Response> loginResponse = RestAssured
            .given()
                .auth().preemptive().basic(configuration.adminUsername(), configuration.adminUserPassword())
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
                .body(objectMapper.writeValueAsString(new SeasonCreationData("test")))
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

    }

    private Map<String, String> createCookieMap(List<String> cookies) {
        Map<String, String> cookieMap = new HashMap<>();
        for (String cookie : cookies) {
            cookieMap.put(cookie.split("=")[0], cookie.split("=")[1].split(";")[0]);
        }
        return cookieMap;
    }
}
