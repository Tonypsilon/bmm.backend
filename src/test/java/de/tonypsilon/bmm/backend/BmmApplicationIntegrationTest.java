package de.tonypsilon.bmm.backend;

import de.tonypsilon.bmm.backend.season.data.SeasonCreationData;
import de.tonypsilon.bmm.backend.season.data.SeasonData;
import de.tonypsilon.bmm.backend.security.AuthenticationResponse;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.web.client.TestRestTemplate;
import org.springframework.http.*;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.test.context.jdbc.Sql;
import org.springframework.test.jdbc.JdbcTestUtils;
import javax.sql.DataSource;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
public class BmmApplicationIntegrationTest {

    final JdbcTemplate jdbcTemplate;

    @Autowired
    BmmApplicationIntegrationTest(DataSource dataSource) {
        this.jdbcTemplate = new JdbcTemplate(dataSource);
    }

    @Autowired
    private TestRestTemplate testRestTemplate;

    @Autowired
    private BmmApplicationIntegrationTestConfiguration configuration;

    @Test
    @Sql(scripts = "classpath:test-user-data.sql",
            executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    @Sql(scripts = "classpath:clear-all-tables.sql",
            executionPhase = Sql.ExecutionPhase.AFTER_TEST_METHOD)
    void modifyDatabaseWithinTransaction() {
        assertEquals(1, JdbcTestUtils.countRowsInTable(jdbcTemplate, "users"));
        assertEquals(1, JdbcTestUtils.countRowsInTable(jdbcTemplate, "authorities"));

        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_JSON);
        headers.setAccept(List.of(MediaType.APPLICATION_JSON));

        // Step 0: Log in as admin and get cookies
        ResponseEntity<AuthenticationResponse> loginResponse = testRestTemplate
                .withBasicAuth(configuration.adminUsername(), configuration.adminUserPassword())
                .getForEntity("/user", AuthenticationResponse.class);

        Map<String, String> loginCookies = createCookieMap(loginResponse.getHeaders().get("Set-Cookie"));

        headers.add("X-XSRF-TOKEN", loginCookies.get("XSRF-TOKEN"));
        headers.add("Cookie", "JSESSIONID=" + loginCookies.get("JSESSIONID"));

        // Step 1: Create a new season
        HttpEntity<SeasonCreationData> createSeasonRequestEntity = new HttpEntity<>(
                new SeasonCreationData("season1"), headers);
        ResponseEntity<SeasonData> postResponse = testRestTemplate
                //.withBasicAuth(configuration.adminUsername(), configuration.adminUserPassword())
                .postForEntity("/seasons", createSeasonRequestEntity, SeasonData.class);
                //.exchange("/seasons", HttpMethod.POST, createSeasonRequestEntity, SeasonData.class);
        System.out.println("hello");


        // Step 2: Create a new user and make it a season admin for the season

    }

    private Map<String, String> createCookieMap(List<String> cookies) {
        Map<String, String> cookieMap = new HashMap<>();
        for (String cookie : cookies) {
            cookieMap.put(cookie.split("=")[0], cookie.split("=")[1].split(";")[0]);
        }
        return cookieMap;
    }

}
