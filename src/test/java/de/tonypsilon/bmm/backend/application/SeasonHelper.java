package de.tonypsilon.bmm.backend.application;

import com.fasterxml.jackson.databind.ObjectMapper;
import de.tonypsilon.bmm.backend.division.data.DivisionCreationData;
import de.tonypsilon.bmm.backend.division.data.DivisionData;
import de.tonypsilon.bmm.backend.season.data.*;
import de.tonypsilon.bmm.backend.season.service.SeasonStage;
import io.restassured.RestAssured;
import io.restassured.response.Response;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;

import java.util.HashMap;
import java.util.Map;

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

    Map<Integer, PlayingDateData> createNinePlayingDatesForSeason(Long seasonId, HttpHeaders headers) throws Exception {
        Map<Integer, PlayingDateData> playingDates = new HashMap<>();
        for (int i=1; i<=9; i++) {
            playingDates.put(i,
                    createPlayingDate(new PlayingDateCreationData(seasonId, i, "1." + i + ".2024"), headers));
        }
        return playingDates;
    }

    private PlayingDateData createPlayingDate(PlayingDateCreationData playingDateCreationData,
                                              HttpHeaders headers) throws Exception {
        PlayingDateData playingDateData = RestAssured
                .given()
                .headers(headers)
                .body(objectMapper.writeValueAsString(playingDateCreationData))
                .when()
                .post(baseUrl + "/playingdates")
                .then()
                .statusCode(HttpStatus.CREATED.value())
                .extract()
                .response()
                .as(PlayingDateData.class);
        assertThat(playingDateData.seasonId()).isEqualTo(playingDateCreationData.seasonId());
        assertThat(playingDateData.number()).isEqualTo(playingDateCreationData.number());
        assertThat(playingDateData.date()).isEqualTo(playingDateCreationData.date());

        return playingDateData;
    }

    DivisionData createDivision(DivisionCreationData divisionCreationData, HttpHeaders headers) throws Exception {
        DivisionData divisionData = RestAssured
                .given()
                .headers(headers)
                .body(objectMapper.writeValueAsString(divisionCreationData))
                .when()
                .post(baseUrl + "/divisions")
                .then()
                .statusCode(HttpStatus.CREATED.value())
                .extract().response().as(DivisionData.class);
        assertThat(divisionData.name()).isEqualTo(divisionCreationData.name());
        assertThat(divisionData.level()).isEqualTo(divisionCreationData.level());
        assertThat(divisionData.seasonId()).isEqualTo(divisionCreationData.seasonId());
        assertThat(divisionData.numberOfBoards()).isEqualTo(divisionCreationData.numberOfBoards());

        return divisionData;
    }
}
