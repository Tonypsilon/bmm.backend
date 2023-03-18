package de.tonypsilon.bmm.backend.application;

import com.fasterxml.jackson.databind.ObjectMapper;
import de.tonypsilon.bmm.backend.club.data.ClubCreationData;
import de.tonypsilon.bmm.backend.club.data.ClubData;
import de.tonypsilon.bmm.backend.organization.data.OrganizationCreationData;
import de.tonypsilon.bmm.backend.organization.data.OrganizationData;
import de.tonypsilon.bmm.backend.security.rnr.Role;
import de.tonypsilon.bmm.backend.security.rnr.data.ClubAdminData;
import de.tonypsilon.bmm.backend.security.rnr.data.UserData;
import de.tonypsilon.bmm.backend.venue.data.VenueCreationData;
import de.tonypsilon.bmm.backend.venue.data.VenueData;
import io.restassured.RestAssured;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;

import java.util.*;

import static org.assertj.core.api.Assertions.assertThat;

public class ClubHelper {

    private final String baseUrl;

    private final LoginHelper loginHelper;

    private final ObjectMapper objectMapper = new ObjectMapper();

    ClubHelper(final String baseUrl) {
        this.baseUrl = baseUrl;
        this.loginHelper = new LoginHelper(baseUrl);
    }

    Map<ClubData, ClubAdminData> createClubs(HttpHeaders headers) throws Exception {
        Map<ClubData, ClubAdminData> clubs = new HashMap<>();
        List<ClubCreationData> clubsToBeCreated = new ArrayList<>();
        clubsToBeCreated.add(new ClubCreationData("Alpha Club", 1, Boolean.TRUE));
        clubsToBeCreated.add(new ClubCreationData("Beta Club", 2, Boolean.TRUE));
        clubsToBeCreated.add(new ClubCreationData("Gamma Club", 3, Boolean.TRUE));
        clubsToBeCreated.add(new ClubCreationData("Delta Club", 4, Boolean.TRUE));
        clubsToBeCreated.add(new ClubCreationData("Epsilon Club", 5, Boolean.TRUE));
        clubsToBeCreated.add(new ClubCreationData("Lambda Club", 6, Boolean.TRUE));
        clubsToBeCreated.add(new ClubCreationData("Omikron Club", 7, Boolean.TRUE));
        clubsToBeCreated.add(new ClubCreationData("Zeta Club", 8, Boolean.TRUE));
        clubsToBeCreated.add(new ClubCreationData("Omega Club", 9, Boolean.TRUE));
        clubsToBeCreated.add(new ClubCreationData("Eta Club", 10, Boolean.TRUE));
        clubsToBeCreated.add(new ClubCreationData("Phi Club", 11, Boolean.TRUE));

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
                                loginHelper.CLUB_ADMIN_PASSWORD,
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

    /**
     * Creates one organization consisting of the first two given clubs.
     * Continues to create a single organization for each following club.
     * Naming for the single-club-organizations will be equal to the club name.
     * The joint organization will be called SG 'first club name' 'second club name'.
     * @param seasonId
     * @param clubs
     * @return
     */
    List<OrganizationData> createOrganizations(Long seasonId, List<ClubData> clubs) throws Exception {
        List<OrganizationData> organizations = new ArrayList<>();
        ClubData clubDataFirstClub = clubs.get(0);
        ClubData clubDataSecondClub = clubs.get(1);
        OrganizationCreationData jointOrganizationCreationData =
                new OrganizationCreationData(seasonId,
                        "SG %s %s".formatted(clubDataFirstClub.name(), clubDataSecondClub.name()),
                        Set.of(clubDataFirstClub.id(), clubDataSecondClub.id()));
        organizations.add(createOrganization(jointOrganizationCreationData,
                loginHelper.login(clubDataFirstClub)));
        for (int i = 2; i < clubs.size(); i++) {
            ClubData clubData = clubs.get(i);
            organizations.add(createOrganization(
                    new OrganizationCreationData(seasonId, clubData.name(), Set.of(clubData.id())),
                    loginHelper.login(clubData)));
        }
        return organizations;
    }

    private OrganizationData createOrganization(
            OrganizationCreationData organizationCreationData, HttpHeaders headers) throws Exception {
        OrganizationData organizationData = RestAssured
                .given()
                .headers(headers)
                .body(objectMapper.writeValueAsString(organizationCreationData))
                .when()
                .post(baseUrl + "/organizations")
                .then()
                .statusCode(HttpStatus.CREATED.value())
                .extract()
                .response()
                .as(OrganizationData.class);
        assertThat(organizationData.name()).isEqualTo(organizationCreationData.name());
        assertThat(organizationData.seasonId()).isEqualTo(organizationCreationData.seasonId());
        assertThat(organizationData.clubIds()).isEqualTo(organizationCreationData.clubIds());

        return organizationData;
    }

    Map<ClubData, VenueData> createVenuesForClubs(Collection<ClubData> clubs) throws Exception {
        Map<ClubData, VenueData> venues = new HashMap<>();
        for (ClubData club : clubs) {
            venues.put(club, createVenue(club));
        }
        return venues;
    }

    private VenueData createVenue(ClubData clubData) throws Exception {
        VenueData venueData = RestAssured
                .given()
                .headers(loginHelper.login(clubData))
                .body(objectMapper.writeValueAsString(
                        new VenueCreationData(clubData.id(),
                                clubData.name() + "location 1",
                                "Hint for " + clubData.name())))
                .when()
                .post(baseUrl + "/venues")
                .then()
                .statusCode(HttpStatus.CREATED.value())
                .extract()
                .response()
                .as(VenueData.class);
        assertThat(venueData.clubId()).isEqualTo(clubData.id());
        assertThat(venueData.address()).isEqualTo(clubData.name() + "location 1");
        assertThat(venueData.hints()).isPresent().hasValue("Hint for " + clubData.name());

        return venueData;
    }
}
