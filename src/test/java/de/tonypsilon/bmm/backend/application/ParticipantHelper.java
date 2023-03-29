package de.tonypsilon.bmm.backend.application;

import com.fasterxml.jackson.databind.ObjectMapper;
import de.tonypsilon.bmm.backend.organization.data.OrganizationData;
import de.tonypsilon.bmm.backend.participant.data.ParticipantCreationData;
import de.tonypsilon.bmm.backend.participant.data.ParticipantData;
import de.tonypsilon.bmm.backend.participationeligibility.data.ParticipationEligibilityCreationData;
import de.tonypsilon.bmm.backend.participationeligibility.data.ParticipationEligibilityData;
import de.tonypsilon.bmm.backend.team.data.TeamData;
import io.restassured.RestAssured;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;

import java.util.*;

import static org.assertj.core.api.Assertions.assertThat;
public class ParticipantHelper {

    private final String baseUrl;

    private final ObjectMapper objectMapper = new ObjectMapper();

    ParticipantHelper(String baseUrl) {
        this.baseUrl = baseUrl;
    }

    List<ParticipationEligibilityData> createParticipationEligibilities(
            Long seasonId, Collection<OrganizationData> organizations, HttpHeaders headers) throws Exception {
        List<ParticipationEligibilityData> participationEligibilities = new ArrayList<>();
        for (OrganizationData organiztation : organizations) {
            participationEligibilities.add(createParticipationEligibility(new ParticipationEligibilityCreationData(
                    seasonId, organiztation.clubIds().iterator().next(), "John", "Doe", 1, 2005), headers));
            participationEligibilities.add(createParticipationEligibility(new ParticipationEligibilityCreationData(
                    seasonId, organiztation.clubIds().iterator().next(), "Jane", "Doe", 2, 1827), headers));
            participationEligibilities.add(createParticipationEligibility(new ParticipationEligibilityCreationData(
                    seasonId, organiztation.clubIds().iterator().next(), "Peter", "Pane", 3, 1901), headers));
            participationEligibilities.add(createParticipationEligibility(new ParticipationEligibilityCreationData(
                    seasonId, organiztation.clubIds().iterator().next(), "Harry", "Hello", 4, 725), headers));
            participationEligibilities.add(createParticipationEligibility(new ParticipationEligibilityCreationData(
                    seasonId, organiztation.clubIds().iterator().next(), "Lara", "Larry", 5, null), headers));
            participationEligibilities.add(createParticipationEligibility(new ParticipationEligibilityCreationData(
                    seasonId, organiztation.clubIds().iterator().next(), "Cinder", "Ella", 6, null), headers));
            participationEligibilities.add(createParticipationEligibility(new ParticipationEligibilityCreationData(
                    seasonId, organiztation.clubIds().iterator().next(), "Cinder", "Ace", 7, null), headers));
            participationEligibilities.add(createParticipationEligibility(new ParticipationEligibilityCreationData(
                    seasonId, organiztation.clubIds().iterator().next(), "Miss", "Ter", 8, null), headers));
            participationEligibilities.add(createParticipationEligibility(new ParticipationEligibilityCreationData(
                    seasonId, organiztation.clubIds().iterator().next(), "Crazy", "Frog", 9, null), headers));
            participationEligibilities.add(createParticipationEligibility(new ParticipationEligibilityCreationData(
                    seasonId, organiztation.clubIds().iterator().next(), "Lucy", "Ferr", 10, null), headers));
            participationEligibilities.add(createParticipationEligibility(new ParticipationEligibilityCreationData(
                    seasonId, organiztation.clubIds().iterator().next(), "Aman A", "Deal", 11, null), headers));
            participationEligibilities.add(createParticipationEligibility(new ParticipationEligibilityCreationData(
                    seasonId, organiztation.clubIds().iterator().next(), "Hugo", "Hurtig", 12, null), headers));
            participationEligibilities.add(createParticipationEligibility(new ParticipationEligibilityCreationData(
                    seasonId, organiztation.clubIds().iterator().next(), "No", "Name", 13, null), headers));
            participationEligibilities.add(createParticipationEligibility(new ParticipationEligibilityCreationData(
                    seasonId, organiztation.clubIds().iterator().next(), "No", "Brainer", 14, null), headers));
            participationEligibilities.add(createParticipationEligibility(new ParticipationEligibilityCreationData(
                    seasonId, organiztation.clubIds().iterator().next(), "Regular", "Frog", 15, null), headers));
            participationEligibilities.add(createParticipationEligibility(new ParticipationEligibilityCreationData(
                    seasonId, organiztation.clubIds().iterator().next(), "Tom", "Ato", 16, null), headers));
        }
        return participationEligibilities;
    }

    private ParticipationEligibilityData createParticipationEligibility(
            ParticipationEligibilityCreationData creationData,
            HttpHeaders headers) throws Exception {
        ParticipationEligibilityData participationEligibility = RestAssured
                .given()
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

    List<ParticipantData> assignParticipantsToTeamsOfOrganization(
            OrganizationData organizationData,
            List<Long> participationEligibilityIds,
            Map<OrganizationData, List<TeamData>> teams,
            HttpHeaders headers) throws Exception {
        List<ParticipantData> participants = new ArrayList<>();
        for (int i = 0; i < 2; i++) {
            participants.addAll(assignParticipantsToTeam(
                    teams.get(organizationData).get(i).id(),
                    participationEligibilityIds.subList(8*i,8*(i+1)),
                    headers));
        }
        return participants;
    }

    private List<ParticipantData> assignParticipantsToTeam(Long teamId,
                                                   List<Long> participationEligibilityIds,
                                                   HttpHeaders headers) throws Exception {
        List<ParticipantData> participants = new ArrayList<>();
        int i = 1;
        for (Long participationEligibilityId : participationEligibilityIds) {
            ParticipantData participant = RestAssured
                    .given()
                    .headers(headers)
                    .body(objectMapper.writeValueAsString(
                            new ParticipantCreationData(teamId, participationEligibilityId, i)))
                    .when()
                    .post(baseUrl + "/participants")
                    .then()
                    .statusCode(HttpStatus.CREATED.value())
                    .extract()
                    .response()
                    .as(ParticipantData.class);
            assertThat(participant.teamId()).isEqualTo(teamId);
            assertThat(participant.participationEligibilityId()).isEqualTo(participationEligibilityId);
            assertThat(participant.number()).isEqualTo(i);

            participants.add(participant);
            i++;
        }

        return participants;
    }

}
