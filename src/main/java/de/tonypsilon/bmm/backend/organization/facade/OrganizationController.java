package de.tonypsilon.bmm.backend.organization.facade;

import de.tonypsilon.bmm.backend.organization.data.OrganizationCreationData;
import de.tonypsilon.bmm.backend.organization.data.OrganizationData;
import de.tonypsilon.bmm.backend.organization.service.OrganizationService;
import de.tonypsilon.bmm.backend.security.rnr.Roles;
import de.tonypsilon.bmm.backend.security.rnr.service.AuthorizationService;
import org.springframework.http.*;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.annotation.security.RolesAllowed;
import java.security.Principal;
import java.util.Objects;

@RestController
public class OrganizationController {

    private final OrganizationService organizationService;
    private final AuthorizationService authorizationService;

    public OrganizationController(final OrganizationService organizationService,
                                  final AuthorizationService authorizationService) {
        this.organizationService = organizationService;
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
        authorizationService.verifyUserIsClubAdminOfAnyClub(principal.getName(), organizationCreationData.clubIds());
        return ResponseEntity
                .status(HttpStatus.CREATED)
                .body(organizationService.createOrganization(organizationCreationData));
    }



}
