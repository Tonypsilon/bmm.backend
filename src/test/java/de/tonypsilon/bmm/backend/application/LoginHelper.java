package de.tonypsilon.bmm.backend.application;

import com.fasterxml.jackson.databind.ObjectMapper;
import io.restassured.RestAssured;
import io.restassured.response.Response;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;

public class LoginHelper {

    private final String baseUrl;

    private final ObjectMapper objectMapper = new ObjectMapper();

    LoginHelper(final String baseUrl) {
        this.baseUrl = baseUrl;
    }

    HttpHeaders login(String username, String password) {
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
