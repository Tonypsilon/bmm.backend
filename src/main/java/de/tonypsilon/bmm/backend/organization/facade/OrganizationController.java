package de.tonypsilon.bmm.backend.organization.facade;

import de.tonypsilon.bmm.backend.organization.service.OrganizationService;
import org.springframework.web.bind.annotation.RestController;

@RestController
public class OrganizationController {

    private final OrganizationService organizationService;

    public OrganizationController(final OrganizationService organizationService) {
        this.organizationService = organizationService;
    }


}
