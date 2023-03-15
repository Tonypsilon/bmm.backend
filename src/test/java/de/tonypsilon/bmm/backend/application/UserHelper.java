package de.tonypsilon.bmm.backend.application;

import com.fasterxml.jackson.databind.ObjectMapper;
import de.tonypsilon.bmm.backend.security.rnr.data.SeasonAdminData;
import de.tonypsilon.bmm.backend.security.rnr.data.UserData;
import io.restassured.RestAssured;
import io.restassured.response.Response;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;

import static org.assertj.core.api.Assertions.assertThat;

public class UserHelper {

    private final String baseUrl;

    private final ObjectMapper objectMapper = new ObjectMapper();

    UserHelper(final String baseUrl) {
        this.baseUrl = baseUrl;
    }

    UserData createUser(UserData userData, HttpHeaders headers) throws Exception {
        Response postUserResponse = RestAssured
                .given()
                .headers(headers)
                .body(objectMapper.writeValueAsString(userData))
                .when()
                .post(baseUrl + "/users")
                .then()
                .statusCode(HttpStatus.CREATED.value())
                .extract()
                .response();

        UserData user = postUserResponse.as(UserData.class);
        assertThat(user.username()).isEqualTo(userData.username());
        assertThat(user.password()).isNull();
        assertThat(user.roles()).isEqualTo(userData.roles());

        return user;
    }

    SeasonAdminData createSeasonAdmin(SeasonAdminData seasonAdminData, HttpHeaders headers)
            throws Exception {
        Response postSeasonAdminResponse = RestAssured
                .given()
                .headers(headers)
                .body(objectMapper.writeValueAsString(seasonAdminData))
                .when()
                .post(baseUrl + "/seasonadmins")
                .then()
                .statusCode(HttpStatus.CREATED.value())
                .extract()
                .response();

        SeasonAdminData seasonAdmin = postSeasonAdminResponse.as(SeasonAdminData.class);
        assertThat(seasonAdmin.seasonId()).isEqualTo(seasonAdminData.seasonId());
        assertThat(seasonAdmin.username()).isEqualTo(seasonAdminData.username());

        return seasonAdmin;
    }
}
