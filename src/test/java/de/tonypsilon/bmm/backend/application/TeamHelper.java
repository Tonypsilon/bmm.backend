package de.tonypsilon.bmm.backend.application;

import com.fasterxml.jackson.databind.ObjectMapper;
import de.tonypsilon.bmm.backend.club.data.ClubData;
import de.tonypsilon.bmm.backend.organization.data.OrganizationData;
import de.tonypsilon.bmm.backend.team.data.TeamCreationData;
import de.tonypsilon.bmm.backend.team.data.TeamData;
import de.tonypsilon.bmm.backend.team.data.TeamDivisionLinkData;
import de.tonypsilon.bmm.backend.venue.data.VenueData;
import io.restassured.RestAssured;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;

import java.util.*;

import static org.assertj.core.api.Assertions.assertThat;

public class TeamHelper {

    private final String baseUrl;
    private final LoginHelper loginHelper;
    private final ObjectMapper objectMapper = new ObjectMapper();

    TeamHelper(final String baseUrl) {
        this.baseUrl = baseUrl;
        this.loginHelper = new LoginHelper(baseUrl);
    }

    Map<OrganizationData, List<TeamData>> createTeams(List<OrganizationData> organizations,
                                                      Map<ClubData, VenueData> venues) throws Exception {
        Map<OrganizationData, List<TeamData>> teams = new HashMap<>();
        for (OrganizationData organizationData : organizations) {
            Optional<ClubData> clubDataOptional = venues.keySet().stream()
                    .filter(clubData -> organizationData.clubIds().contains(clubData.id())).findFirst();
            assertThat(clubDataOptional).isPresent();
            ClubData clubData = clubDataOptional.get();
            VenueData venueData = venues.get(clubData);
            HttpHeaders clubAdminHeaders = loginHelper.login(clubData);
            teams.put(organizationData, List.of(
                    createTeam(new TeamCreationData(organizationData.id(), 1, venueData.id()), clubAdminHeaders),
                    createTeam(new TeamCreationData(organizationData.id(), 2, venueData.id()), clubAdminHeaders)
            ));
        }
        return teams;
    }

    private TeamData createTeam(TeamCreationData teamCreationData, HttpHeaders headers) throws Exception {
         TeamData teamData = RestAssured
                .given()
                .headers(headers)
                .body(objectMapper.writeValueAsString(teamCreationData))
                .when()
                .post(baseUrl + "/teams")
                .then()
                .statusCode(HttpStatus.CREATED.value())
                .extract()
                .response()
                .as(TeamData.class);
         assertThat(teamData.organizationId()).isEqualTo(teamCreationData.organizationId());
         assertThat(teamData.number()).isEqualTo(teamCreationData.number());
         assertThat(teamData.venueId()).isEqualTo(teamCreationData.venueId());

         return teamData;
    }

    private TeamDivisionLinkData createTeamDivisionLink(
            TeamDivisionLinkData teamDivisionLinkData, HttpHeaders headers)  throws Exception {
        TeamDivisionLinkData teamDivisionLink = RestAssured
                .given()
                .headers(headers)
                .body(objectMapper.writeValueAsString(teamDivisionLinkData))
                .when()
                .post(baseUrl + "/teamdivisionlinks")
                .then()
                .statusCode(HttpStatus.CREATED.value())
                .extract()
                .response()
                .as(TeamDivisionLinkData.class);
        assertThat(teamDivisionLink.teamId()).isEqualTo(teamDivisionLinkData.teamId());
        assertThat(teamDivisionLink.divisionId()).isEqualTo(teamDivisionLinkData.divisionId());
        assertThat(teamDivisionLink.number()).isEqualTo(teamDivisionLinkData.number());

        return teamDivisionLink;
    }
}
