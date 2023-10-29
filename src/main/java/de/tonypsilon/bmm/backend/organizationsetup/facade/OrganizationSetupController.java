package de.tonypsilon.bmm.backend.organizationsetup.facade;

import de.tonypsilon.bmm.backend.organization.data.OrganizationData;
import de.tonypsilon.bmm.backend.organization.service.OrganizationService;
import de.tonypsilon.bmm.backend.organizationsetup.data.OrganizationSetupData;
import de.tonypsilon.bmm.backend.organizationsetup.data.TeamSetupData;
import de.tonypsilon.bmm.backend.organizationsetup.service.OrganizationSetupService;
import de.tonypsilon.bmm.backend.participationeligibility.data.ParticipationEligibilityData;
import de.tonypsilon.bmm.backend.participationeligibility.service.ParticipationEligibilityService;
import de.tonypsilon.bmm.backend.security.rnr.Roles;
import de.tonypsilon.bmm.backend.security.rnr.service.AuthorizationService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.MediaType;
import org.springframework.http.RequestEntity;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.annotation.security.RolesAllowed;
import java.security.Principal;
import java.util.Collection;
import java.util.List;
import java.util.Objects;
import java.util.function.Predicate;

@RestController
public class OrganizationSetupController {

    private final Logger logger = LoggerFactory.getLogger(OrganizationSetupController.class);
    private final OrganizationSetupService organizationSetupService;
    private final ParticipationEligibilityService participationEligibilityService;
    private final OrganizationService organizationService;
    private final AuthorizationService authorizationService;

    public OrganizationSetupController(final OrganizationSetupService organizationSetupService,
                                       final ParticipationEligibilityService participationEligibilityService,
                                       final OrganizationService organizationService,
                                       final AuthorizationService authorizationService) {
        this.organizationSetupService = organizationSetupService;
        this.participationEligibilityService = participationEligibilityService;
        this.organizationService = organizationService;
        this.authorizationService = authorizationService;
    }

    @RolesAllowed(Roles.CLUB_ADMIN)
    @GetMapping(value = "/organizations/{organizationId}/setup", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<OrganizationSetupData> getOrganizationSetup(Principal principal,
                                                                      @PathVariable Long organizationId) {
        authorizationService.verifyUserIsClubAdminOfOrganization(principal.getName(),
                Objects.requireNonNull(organizationId));
        OrganizationData organizationData = organizationService.getOrganizationById(organizationId);
        List<ParticipationEligibilityData> availablePlayers =
                organizationData.clubIds().stream()
                        .map(clubId -> participationEligibilityService
                                .getAllParticipationEligibilitiesForSeasonAndClub(
                                        organizationData.seasonId(), clubId))
                        .flatMap(Collection::stream)
                        .toList();
        List<TeamSetupData> teams = organizationSetupService.getOrganizationSetup(organizationId);
        List<ParticipationEligibilityData> participantsInTeams = teams.stream()
                .map(TeamSetupData::participants)
                .flatMap(List::stream)
                .toList();
        return ResponseEntity
                .ok(new OrganizationSetupData(
                        availablePlayers.stream()
                                .filter(Predicate.not(participantsInTeams::contains))
                                .toList(),
                        teams,
                        organizationData.firstTeamNumber())
                );
    }

    @RolesAllowed(Roles.CLUB_ADMIN)
    @PutMapping(value = "/organizations/{organizationId}/setup",
            consumes = MediaType.APPLICATION_JSON_VALUE,
            produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<List<TeamSetupData>> putOrganizationSetup(
            RequestEntity<List<TeamSetupData>> teamsSetupDataRequestEntity,
            Principal principal,
            @PathVariable Long organizationId) {
        authorizationService.verifyUserIsClubAdminOfOrganization(principal.getName(),
                Objects.requireNonNull(organizationId));
        List<TeamSetupData> teamsSetupData = Objects.requireNonNull(teamsSetupDataRequestEntity).getBody();
        Objects.requireNonNull(teamsSetupData);
        logger.info("User %s, PUT on /organizations/%d/setup, body: %s"
                .formatted(principal.getName(), organizationId, teamsSetupData));
        return ResponseEntity
                .ok(organizationSetupService.setUpTeamsOfOrganization(
                        organizationId,
                        teamsSetupData.stream().filter(Objects::nonNull).toList()));
    }
}
