package de.tonypsilon.bmm.backend.application;

import com.fasterxml.jackson.databind.ObjectMapper;
import de.tonypsilon.bmm.backend.season.data.SeasonCreationData;
import de.tonypsilon.bmm.backend.season.data.SeasonData;
import de.tonypsilon.bmm.backend.season.service.SeasonStage;
import io.restassured.RestAssured;
import io.restassured.response.Response;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;

import static org.assertj.core.api.Assertions.assertThat;

public class SeasonHelper {

    private final ObjectMapper objectMapper = new ObjectMapper();
    private final String baseUrl;

    SeasonHelper(String baseUrl) {
        this.baseUrl = baseUrl;
    }

    SeasonData createSeason(SeasonCreationData seasonCreationData,
                            HttpHeaders headers) throws Exception {
        Response postSeasonResponse = RestAssured
                .given()
                .headers(headers)
                .body(objectMapper.writeValueAsString(seasonCreationData))
                .when()
                .post(baseUrl + "/seasons")
                .then()
                .statusCode(HttpStatus.CREATED.value())
                .extract()
                .response();

        SeasonData seasonData = postSeasonResponse.as(SeasonData.class);
        assertThat(seasonData.name()).isEqualTo(seasonCreationData.name());
        assertThat(seasonData.stage()).isEqualTo(SeasonStage.REGISTRATION);

        return seasonData;
    }
}
