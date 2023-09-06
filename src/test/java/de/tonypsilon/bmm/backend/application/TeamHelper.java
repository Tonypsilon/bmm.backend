package de.tonypsilon.bmm.backend.application;

import com.fasterxml.jackson.databind.ObjectMapper;
import de.tonypsilon.bmm.backend.club.data.ClubData;
import de.tonypsilon.bmm.backend.division.data.DivisionData;
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
                    createTeam(new TeamCreationData(
                            organizationData.id(), 1, venueData.id(), null, "admin"),
                            clubAdminHeaders),
                    createTeam(new TeamCreationData(
                            organizationData.id(), 2, venueData.id(), null, "admin"),
                            clubAdminHeaders)
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

    Map<DivisionData, List<TeamDivisionLinkData>> linkTeamsToDivisions(
            Map<OrganizationData, List<TeamData>> teams,
            DivisionData division1,
            DivisionData division2,
            HttpHeaders headers) throws Exception{
        Map<DivisionData, List<TeamDivisionLinkData>> teamLinks = new HashMap<>();
        teamLinks.put(division1, new ArrayList<>());
        teamLinks.put(division2, new ArrayList<>());
        int i = 1;
        for (OrganizationData organizationData : teams.keySet()) {
            teamLinks.get(division1).add(createTeamDivisionLink(
                    new TeamDivisionLinkData(teams.get(organizationData).get(0).id(), division1.id(), i), headers));
            teamLinks.get(division2).add(createTeamDivisionLink(
                    new TeamDivisionLinkData(teams.get(organizationData).get(1).id(), division2.id(), i), headers));
            i++;
        }
        assertThat(teamLinks.get(division1)).isNotNull().hasSize(10);
        assertThat(teamLinks.get(division2)).isNotNull().hasSize(10);
        return teamLinks;
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
