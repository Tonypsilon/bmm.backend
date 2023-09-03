package de.tonypsilon.bmm.backend.organizationsetup.service;

import de.tonypsilon.bmm.backend.exception.BadDataException;
import de.tonypsilon.bmm.backend.exception.SeasonStageException;
import de.tonypsilon.bmm.backend.organization.data.OrganizationData;
import de.tonypsilon.bmm.backend.organization.service.OrganizationService;
import de.tonypsilon.bmm.backend.organizationsetup.data.TeamSetupData;
import de.tonypsilon.bmm.backend.participant.service.ParticipantService;
import de.tonypsilon.bmm.backend.participationeligibility.data.ParticipationEligibilityData;
import de.tonypsilon.bmm.backend.participationeligibility.service.ParticipationEligibilityService;
import de.tonypsilon.bmm.backend.season.service.SeasonService;
import de.tonypsilon.bmm.backend.season.service.SeasonStage;
import de.tonypsilon.bmm.backend.team.data.TeamCreationData;
import de.tonypsilon.bmm.backend.team.data.TeamData;
import de.tonypsilon.bmm.backend.team.service.TeamService;
import de.tonypsilon.bmm.backend.venue.data.VenueData;
import de.tonypsilon.bmm.backend.venue.service.VenueService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;

import java.util.Collections;
import java.util.List;
import java.util.Set;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatExceptionOfType;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class OrganizationSetupServiceTest {

    private final TeamService teamService = mock(TeamService.class);
    private final OrganizationService organizationService = mock(OrganizationService.class);
    private final SeasonService seasonService = mock(SeasonService.class);
    private final VenueService venueService = mock(VenueService.class);
    private final ParticipationEligibilityService participationEligibilityService =
            mock(ParticipationEligibilityService.class);
    private final ParticipantService participantService = mock(ParticipantService.class);
    private OrganizationSetupService organizationSetupService;

    private final Long organizationId = 1L;
    private final OrganizationData organizationData =
            new OrganizationData(1L, 2L, "org", Set.of(3L));

    private final ParticipationEligibilityData p1 =
            new ParticipationEligibilityData(1L, 2L, 3L, "fore", "sur", 3, null);
    private final ParticipationEligibilityData p2 =
            new ParticipationEligibilityData(2L, 2L, 3L, "fore", "sur", 3, null);
    private final ParticipationEligibilityData p3 =
            new ParticipationEligibilityData(3L, 2L, 3L, "fore", "sur", 3, null);
    private final ParticipationEligibilityData p4 =
            new ParticipationEligibilityData(4L, 2L, 3L, "fore", "sur", 3, null);
    private final ParticipationEligibilityData p5 =
            new ParticipationEligibilityData(5L, 2L, 3L, "fore", "sur", 3, null);
    private final ParticipationEligibilityData p6 =
            new ParticipationEligibilityData(6L, 2L, 3L, "fore", "sur", 3, null);

    @BeforeEach
    void setUp() {
        this.organizationSetupService = new OrganizationSetupService(
                teamService,
                organizationService,
                seasonService,
                venueService,
                participationEligibilityService,
                participantService);
    }

    @Test
    void testSetUpTeamsOfOrganizationWrongOrganization() {
        TeamSetupData teamSetupData1 = new TeamSetupData(
                organizationId, 1, 1L, null, Collections.emptyList());
        TeamSetupData teamSetupData2 = new TeamSetupData(
                2L, 2, 1L, null, Collections.emptyList());
        assertThatExceptionOfType(BadDataException.class)
                .isThrownBy(() -> organizationSetupService.setUpTeamsOfOrganization(organizationId,
                        List.of(teamSetupData1, teamSetupData2)))
                .withMessage("Alle Teams müssen zur gegebenen Organisation gehören!");
    }

    @Test
    void testSetUpTeamsOfOrganizationDuplicates() {
        TeamSetupData teamSetupData1 = new TeamSetupData(organizationId, 1, 1L, null,
                List.of(p1, p2, p3));
        TeamSetupData teamSetupData2 = new TeamSetupData(organizationId, 2, 1L, null,
                List.of(p3, p4, p5));
        assertThatExceptionOfType(BadDataException.class)
                .isThrownBy(() -> organizationSetupService.setUpTeamsOfOrganization(organizationId,
                        List.of(teamSetupData1, teamSetupData2)))
                .withMessage("Es darf kein Spieler in mehreren Mannschaften vorkommen!");
    }

    @ParameterizedTest
    @EnumSource(value = SeasonStage.class,
            mode = EnumSource.Mode.EXCLUDE,
            names = {"REGISTRATION"})
    void testSetUpTeamsOfOrganizationWrongSeasonStage(SeasonStage seasonStage) {
        TeamSetupData teamSetupData1 = new TeamSetupData(organizationId, 1, 1L, null,
                List.of(p1, p2, p3));
        TeamSetupData teamSetupData2 = new TeamSetupData(organizationId, 2, 1L, null,
                List.of(p4, p5, p6));
        when(organizationService.getOrganizationById(organizationId)).thenReturn(organizationData);
        when(seasonService.getStageOfSeason(organizationData.seasonId())).thenReturn(seasonStage);
        assertThatExceptionOfType(SeasonStageException.class)
                .isThrownBy(() -> organizationSetupService.setUpTeamsOfOrganization(organizationId,
                        List.of(teamSetupData1, teamSetupData2)))
                .withMessage("Saison ist nicht in der Registrierungsphase!");
    }

    @Test
    void testSetUpTeamsOfOrganizationBadTeamNumbers() {
        TeamSetupData teamSetupData1 = new TeamSetupData(organizationId, 1, 1L, null,
                List.of(p1, p2, p3));
        TeamSetupData teamSetupData2 = new TeamSetupData(organizationId, 3, 1L, null,
                List.of(p4, p5, p6));
        when(organizationService.getOrganizationById(organizationId)).thenReturn(organizationData);
        when(seasonService.getStageOfSeason(organizationData.seasonId())).thenReturn(SeasonStage.REGISTRATION);
        assertThatExceptionOfType(BadDataException.class)
                .isThrownBy(() -> organizationSetupService.setUpTeamsOfOrganization(organizationId,
                        List.of(teamSetupData1, teamSetupData2)))
                .withMessage("Die Teamnummern passen nicht zusammen oder sind nicht sortiert!");
    }

    @Test
    void testSetUpTeamsOfOrganizationInvalidParticipationEligibility() {
        TeamSetupData teamSetupData1 = new TeamSetupData(organizationId, 1, 1L, null,
                List.of(p1, p2));
        TeamSetupData teamSetupData2 = new TeamSetupData(organizationId, 2, 1L, null,
                List.of(p4, p5));
        when(organizationService.getOrganizationById(organizationId)).thenReturn(organizationData);
        when(seasonService.getStageOfSeason(organizationData.seasonId())).thenReturn(SeasonStage.REGISTRATION);
        when(participationEligibilityService.isValidParticipationEligibilityForOrganization(organizationData))
                .thenReturn(i -> i < 5L);
        assertThatExceptionOfType(BadDataException.class)
                .isThrownBy(() -> organizationSetupService.setUpTeamsOfOrganization(organizationId,
                        List.of(teamSetupData1, teamSetupData2)))
                .withMessage("Mindestens ein Spieler hat nicht die notwendige Spielberechtigung!");
    }

    @Test
    void testSetUpTeamsOfOrganizationTooLargeTeam() {
        TeamSetupData teamSetupData1 = new TeamSetupData(organizationId, 1, 1L, null,
                List.of(p1, p2, p3, p4, p5, p6,
                        new ParticipationEligibilityData(7L, 2L, 3L, "fore", "sur", 3, null),
                        new ParticipationEligibilityData(8L, 2L, 3L, "fore", "sur", 3, null),
                        new ParticipationEligibilityData(9L, 2L, 3L, "fore", "sur", 3, null),
                        new ParticipationEligibilityData(10L, 2L, 3L, "fore", "sur", 3, null),
                        new ParticipationEligibilityData(11L, 2L, 3L, "fore", "sur", 3, null),
                        new ParticipationEligibilityData(12L, 2L, 3L, "fore", "sur", 3, null),
                        new ParticipationEligibilityData(13L, 2L, 3L, "fore", "sur", 3, null),
                        new ParticipationEligibilityData(14L, 2L, 3L, "fore", "sur", 3, null),
                        new ParticipationEligibilityData(15L, 2L, 3L, "fore", "sur", 3, null),
                        new ParticipationEligibilityData(16L, 2L, 3L, "fore", "sur", 3, null),
                        new ParticipationEligibilityData(17L, 2L, 3L, "fore", "sur", 3, null)));
        TeamSetupData teamSetupData2 = new TeamSetupData(organizationId, 2, 1L, null,
                List.of(new ParticipationEligibilityData(18L, 2L, 3L, "fore", "sur", 3, null),
                        new ParticipationEligibilityData(19L, 2L, 3L, "fore", "sur", 3, null)));
        when(organizationService.getOrganizationById(organizationId)).thenReturn(organizationData);
        when(seasonService.getStageOfSeason(organizationData.seasonId())).thenReturn(SeasonStage.REGISTRATION);
        when(participationEligibilityService.isValidParticipationEligibilityForOrganization(organizationData))
                .thenReturn(i -> i < 25L);
        assertThatExceptionOfType(BadDataException.class)
                .isThrownBy(() -> organizationSetupService.setUpTeamsOfOrganization(organizationId,
                        List.of(teamSetupData1, teamSetupData2)))
                .withMessage("Kein Team darf mehr als 16 Spieler haben!");
    }

    @Test
    void testSetUpTeamsOfOrganizationMissingVenue() {
        TeamSetupData teamSetupData1 = new TeamSetupData(organizationId, 1, 1L, null,
                List.of(p1, p2, p3));
        TeamSetupData teamSetupData2 = new TeamSetupData(organizationId, 2, 1L, null,
                List.of(p4, p5, p6));
        when(organizationService.getOrganizationById(organizationId)).thenReturn(organizationData);
        when(seasonService.getStageOfSeason(organizationData.seasonId())).thenReturn(SeasonStage.REGISTRATION);
        when(participationEligibilityService.isValidParticipationEligibilityForOrganization(organizationData))
                .thenReturn(i -> i < 7L);
        when(venueService.getVenuesForOrganization(organizationId))
                .thenReturn(List.of(new VenueData(2L, 3L, "address", "hints")));
        assertThatExceptionOfType(BadDataException.class)
                .isThrownBy(() -> organizationSetupService.setUpTeamsOfOrganization(organizationId,
                        List.of(teamSetupData1, teamSetupData2)))
                .withMessage("Alle Mannschaften müssen einem Spielort der Organisation zugeordnet sein!");
    }

    @Test
    void testSetUpTeamsOfOrganizationSuccess() {
        TeamSetupData teamSetupData1 = new TeamSetupData(organizationId, 1, 1L, null,
                List.of(p1, p2, p3));
        TeamSetupData teamSetupData2 = new TeamSetupData(organizationId, 2, 1L, null,
                List.of(p4, p5, p6));
        when(organizationService.getOrganizationById(organizationId)).thenReturn(organizationData);
        when(seasonService.getStageOfSeason(organizationData.seasonId())).thenReturn(SeasonStage.REGISTRATION);
        when(participationEligibilityService.isValidParticipationEligibilityForOrganization(organizationData))
                .thenReturn(i -> i < 7L);
        when(venueService.getVenuesForOrganization(organizationId))
                .thenReturn(List.of(new VenueData(1L, 3L, "address", "hints")));
        when(teamService.createTeam(new TeamCreationData(organizationId, 1, 1L)))
                .thenReturn(new TeamData(1L, 1L, 1, 1L));
        when(teamService.createTeam(new TeamCreationData(organizationId, 2, 1L)))
                .thenReturn(new TeamData(2L, 1L, 2, 1L));
        List<TeamSetupData> actual = organizationSetupService.setUpTeamsOfOrganization(organizationId,
                List.of(teamSetupData1, teamSetupData2));
        assertThat(actual).isEqualTo(List.of(teamSetupData1, teamSetupData2));
        // TODO: verify service calls for deletion and for creation of teams / participants
    }

}