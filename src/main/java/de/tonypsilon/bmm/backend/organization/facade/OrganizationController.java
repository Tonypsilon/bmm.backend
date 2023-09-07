package de.tonypsilon.bmm.backend.organization.facade;

import de.tonypsilon.bmm.backend.datatypes.IdAndLabel;
import de.tonypsilon.bmm.backend.organization.data.OrganizationCreationData;
import de.tonypsilon.bmm.backend.organization.data.OrganizationData;
import de.tonypsilon.bmm.backend.organization.service.OrganizationAdminService;
import de.tonypsilon.bmm.backend.organization.service.OrganizationService;
import de.tonypsilon.bmm.backend.season.service.SeasonService;
import de.tonypsilon.bmm.backend.season.service.SeasonStage;
import de.tonypsilon.bmm.backend.security.rnr.Roles;
import de.tonypsilon.bmm.backend.security.rnr.service.AuthorizationService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.RequestEntity;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.annotation.security.RolesAllowed;
import java.security.Principal;
import java.util.List;
import java.util.Objects;
import java.util.Set;

@RestController
public class OrganizationController {

    private final Logger logger = LoggerFactory.getLogger(OrganizationController.class);
    private final OrganizationService organizationService;
    private final OrganizationAdminService organizationAdminService;
    private final SeasonService seasonService;
    private final AuthorizationService authorizationService;

    public OrganizationController(final OrganizationService organizationService,
                                  final OrganizationAdminService organizationAdminService,
                                  final SeasonService seasonService,
                                  final AuthorizationService authorizationService) {
        this.organizationService = organizationService;
        this.organizationAdminService = organizationAdminService;
        this.seasonService = seasonService;
        this.authorizationService = authorizationService;
    }

    @RolesAllowed(Roles.CLUB_ADMIN)
    @PostMapping(value = "/organizations",
            consumes = MediaType.APPLICATION_JSON_VALUE,
            produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<OrganizationData> createOrganization(
            RequestEntity<OrganizationCreationData> organizationCreationDataRequestEntity,
            Principal principal) {
        OrganizationCreationData organizationCreationData =
                Objects.requireNonNull(organizationCreationDataRequestEntity.getBody());
        Objects.requireNonNull(organizationCreationData);
        logger.info("User %s, POST on /organizations, body: %s"
                .formatted(principal.getName(), organizationCreationData));
        authorizationService.verifyUserIsClubAdminOfAnyClub(principal.getName(), organizationCreationData.clubIds());
        return ResponseEntity
                .status(HttpStatus.CREATED)
                .body(organizationService.createOrganization(organizationCreationData));
    }

    @RolesAllowed(Roles.CLUB_ADMIN)
    @GetMapping(value = "/organizations/registration", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<List<IdAndLabel>> getOrganizationsInRegistrationStageForUser(Principal principal) {
        return ResponseEntity.ok(
                organizationAdminService.getOrganizationsOfUser(principal.getName())
                        .stream()
                        .filter(organizationData -> seasonService.getStageOfSeason(organizationData.seasonId())
                                == SeasonStage.REGISTRATION)
                        .map(organizationData ->
                                new IdAndLabel(organizationData.id(),
                                        organizationData.name() + " - "
                                                + seasonService.getSeasonById(organizationData.seasonId()).name()))
                        .toList()
        );
    }

}
