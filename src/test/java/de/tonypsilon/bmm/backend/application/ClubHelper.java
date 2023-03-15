package de.tonypsilon.bmm.backend.application;

import com.fasterxml.jackson.databind.ObjectMapper;
import de.tonypsilon.bmm.backend.club.data.ClubCreationData;
import de.tonypsilon.bmm.backend.club.data.ClubData;
import de.tonypsilon.bmm.backend.security.rnr.Role;
import de.tonypsilon.bmm.backend.security.rnr.data.ClubAdminData;
import de.tonypsilon.bmm.backend.security.rnr.data.UserData;
import io.restassured.RestAssured;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;

import java.util.*;

import static org.assertj.core.api.Assertions.assertThat;

public class ClubHelper {

    private final String baseUrl;

    private static final String CLUB_ADMIN_PASSWORD = "secret";

    private final ObjectMapper objectMapper = new ObjectMapper();

    ClubHelper(final String baseUrl) {
        this.baseUrl = baseUrl;
    }

    Map<ClubData, ClubAdminData> createClubs(HttpHeaders headers) throws Exception {
        Map<ClubData, ClubAdminData> clubs = new HashMap<>();
        List<ClubCreationData> clubsToBeCreated = new ArrayList<>();


        for (ClubCreationData clubCreationData : clubsToBeCreated) {
            ClubData clubData = createClub(clubCreationData, headers);
            clubs.put(clubData, createClubAdmin(clubData, headers));
        }
        return clubs;
    }

    private ClubData createClub(ClubCreationData clubCreationData, HttpHeaders headers) throws Exception {
        ClubData clubData = RestAssured
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
        assertThat(clubData.name()).isEqualTo(clubCreationData.name());
        assertThat(clubData.zps()).isEqualTo(clubCreationData.zps());
        assertThat(clubData.active()).isTrue();

        return clubData;
    }

    private ClubAdminData createClubAdmin(ClubData clubData, HttpHeaders headers) throws Exception {
        UserData userData = RestAssured
                .given()
                .headers(headers)
                .body(objectMapper.writeValueAsString(
                        new UserData(clubData.name() + "Admin",
                                CLUB_ADMIN_PASSWORD,
                                Set.of(Role.CLUB_ADMIN))))
                .when()
                .post(baseUrl + "/users")
                .then()
                .statusCode(HttpStatus.CREATED.value())
                .extract()
                .response()
                .as(UserData.class);
        assertThat(userData.username()).isEqualTo(clubData.name() + "Admin");
        assertThat(userData.roles()).containsExactlyInAnyOrder(Role.CLUB_ADMIN);

        ClubAdminData clubAdminData = RestAssured
                .given()
                .headers(headers)
                .body(objectMapper.writeValueAsString(
                        new ClubAdminData(clubData.id(), clubData.name() + "Admin")))
                .when()
                .post(baseUrl + "/clubadmins")
                .then()
                .statusCode(HttpStatus.CREATED.value())
                .extract()
                .response()
                .as(ClubAdminData.class);
        assertThat(clubAdminData.clubId()).isEqualTo(clubData.id());
        assertThat(clubAdminData.username()).isEqualTo(clubData.name() + "Admin");

        return clubAdminData;
    }
}
