package de.tonypsilon.bmm.backend.application;

import com.fasterxml.jackson.databind.ObjectMapper;
import de.tonypsilon.bmm.backend.club.data.ClubData;
import de.tonypsilon.bmm.backend.participationeligibility.data.ParticipationEligibilityCreationData;
import de.tonypsilon.bmm.backend.participationeligibility.data.ParticipationEligibilityData;
import io.restassured.RestAssured;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
public class ParticipantHelper {

    private final String baseUrl;

    private final ObjectMapper objectMapper = new ObjectMapper();

    ParticipantHelper(String baseUrl) {
        this.baseUrl = baseUrl;
    }

    List<ParticipationEligibilityData> createParticipationEligibilities(
            Long seasonId, Collection<ClubData> clubs, HttpHeaders headers) throws Exception {
        List<ParticipationEligibilityData> participationEligibilities = new ArrayList<>();
        for (ClubData club : clubs) {
            participationEligibilities.add(createParticipationEligibility(new ParticipationEligibilityCreationData(
                    seasonId, club.id(), "John", "Doe", 1, 2005), headers));
            participationEligibilities.add(createParticipationEligibility(new ParticipationEligibilityCreationData(
                    seasonId, club.id(), "Jane", "Doe", 2, 1827), headers));
            participationEligibilities.add(createParticipationEligibility(new ParticipationEligibilityCreationData(
                    seasonId, club.id(), "Peter", "Pane", 3, 1901), headers));
            participationEligibilities.add(createParticipationEligibility(new ParticipationEligibilityCreationData(
                    seasonId, club.id(), "Harry", "Hello", 4, 725), headers));
            participationEligibilities.add(createParticipationEligibility(new ParticipationEligibilityCreationData(
                    seasonId, club.id(), "Lara", "Larry", 5, null), headers));
            participationEligibilities.add(createParticipationEligibility(new ParticipationEligibilityCreationData(
                    seasonId, club.id(), "Cinder", "Ella", 6, null), headers));
            participationEligibilities.add(createParticipationEligibility(new ParticipationEligibilityCreationData(
                    seasonId, club.id(), "Cinder", "Ace", 7, null), headers));
            participationEligibilities.add(createParticipationEligibility(new ParticipationEligibilityCreationData(
                    seasonId, club.id(), "Miss", "Ter", 8, null), headers));
            participationEligibilities.add(createParticipationEligibility(new ParticipationEligibilityCreationData(
                    seasonId, club.id(), "Crazy", "Frog", 9, null), headers));
            participationEligibilities.add(createParticipationEligibility(new ParticipationEligibilityCreationData(
                    seasonId, club.id(), "Lucy", "Ferr", 10, null), headers));
            participationEligibilities.add(createParticipationEligibility(new ParticipationEligibilityCreationData(
                    seasonId, club.id(), "Aman A", "Deal", 11, null), headers));
            participationEligibilities.add(createParticipationEligibility(new ParticipationEligibilityCreationData(
                    seasonId, club.id(), "Hugo", "Hurtig", 12, null), headers));
            participationEligibilities.add(createParticipationEligibility(new ParticipationEligibilityCreationData(
                    seasonId, club.id(), "No", "Name", 13, null), headers));
            participationEligibilities.add(createParticipationEligibility(new ParticipationEligibilityCreationData(
                    seasonId, club.id(), "No", "Brainer", 14, null), headers));
            participationEligibilities.add(createParticipationEligibility(new ParticipationEligibilityCreationData(
                    seasonId, club.id(), "Regular", "Frog", 15, null), headers));
            participationEligibilities.add(createParticipationEligibility(new ParticipationEligibilityCreationData(
                    seasonId, club.id(), "Tom", "Ato", 16, null), headers));
        }
        return participationEligibilities;
    }

    private ParticipationEligibilityData createParticipationEligibility(
            ParticipationEligibilityCreationData creationData,
            HttpHeaders headers) throws Exception {
        ParticipationEligibilityData participationEligibility = RestAssured
                .given()
                .log().all()
                .headers(headers)
                .body(objectMapper.writeValueAsString(creationData))
                .when()
                .post(baseUrl + "/participationeligibilities")
                .then()
                .statusCode(HttpStatus.CREATED.value())
                .extract()
                .response()
                .as(ParticipationEligibilityData.class);
        assertThat(participationEligibility.clubId()).isEqualTo(creationData.clubId());
        assertThat(participationEligibility.seasonId()).isEqualTo(creationData.seasonId());
        assertThat(participationEligibility.forename()).isEqualTo(creationData.forename());
        assertThat(participationEligibility.surname()).isEqualTo(creationData.surname());
        assertThat(participationEligibility.pkz()).isEqualTo(creationData.pkz());
        assertThat(participationEligibility.dwz()).isEqualTo(Optional.ofNullable(creationData.dwz()));

        return participationEligibility;
    }

}
