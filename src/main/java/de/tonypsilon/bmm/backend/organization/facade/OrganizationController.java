package de.tonypsilon.bmm.backend.organization.facade;

import de.tonypsilon.bmm.backend.organization.data.OrganizationCreationData;
import de.tonypsilon.bmm.backend.organization.data.OrganizationData;
import de.tonypsilon.bmm.backend.organization.service.OrganizationService;
import de.tonypsilon.bmm.backend.security.rnr.Roles;
import de.tonypsilon.bmm.backend.security.rnr.service.ClubAdminService;
import org.springframework.http.*;
import org.springframework.lang.NonNull;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.annotation.security.RolesAllowed;
import java.security.Principal;
import java.util.Objects;
import java.util.Set;

@RestController
public class OrganizationController {

    private final OrganizationService organizationService;
    private final ClubAdminService clubAdminService;

    public OrganizationController(final OrganizationService organizationService,
                                  final ClubAdminService clubAdminService) {
        this.organizationService = organizationService;
        this.clubAdminService = clubAdminService;
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
        verifyUserIsClubAdminOfAnyOrganizationMember(principal.getName(),
                organizationCreationData.clubIds());
        return ResponseEntity
                .status(HttpStatus.CREATED)
                .body(organizationService.createOrganization(organizationCreationData));
    }

    private void verifyUserIsClubAdminOfAnyOrganizationMember(@NonNull String username, @NonNull Set<Long> clubIds) {
        if(clubIds.stream()
                .map(clubAdminService::getAdminsOfClub)
                .flatMap(Set::stream)
                .noneMatch(username::equals)) {
            throw new AccessDeniedException("Benutzer %s ist kein Admin für einen der gegebenen Vereine!"
                    .formatted(username));
        }
    }

}
