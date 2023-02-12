package de.tonypsilon.bmm.backend.venue.service;

import de.tonypsilon.bmm.backend.exception.AlreadyExistsException;
import de.tonypsilon.bmm.backend.organization.service.OrganizationService;
import de.tonypsilon.bmm.backend.venue.data.*;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
public class VenueOrganizationLinkService {

    private final VenueOrganizationLinkRepository venueOrganizationLinkRepository;
    private final VenueService venueService;
    private final OrganizationService organizationService;

    public VenueOrganizationLinkService(
            final VenueOrganizationLinkRepository venueOrganizationLinkRepository,
            final VenueService venueService,
            final OrganizationService organizationService) {
        this.venueOrganizationLinkRepository = venueOrganizationLinkRepository;
        this.venueService = venueService;
        this.organizationService = organizationService;
    }

    @Transactional
    @NonNull
    public VenueOrganizationLinkData createVenueOrganizationLink(
            VenueOrganizationLinkData creationData) {
        venueService.verifyVenueExistsById(creationData.venueId());
        organizationService.verifyOrganizationExistsById(creationData.organizationId());
        if (venueOrganizationLinkRepository
                .existsByVenueIdAndOrganizationId(creationData.venueId(), creationData.organizationId())) {
            throw new AlreadyExistsException("Der Spielort %d ist bereits der Organisation %d zugeordnet!"
                    .formatted(creationData.venueId(), creationData.organizationId()));
        }
        VenueOrganizationLink venueOrganizationLink = new VenueOrganizationLink();
        venueOrganizationLink.setVenueId(creationData.venueId());
        venueOrganizationLink.setOrganizationId(creationData.organizationId());
        venueOrganizationLinkRepository.save(venueOrganizationLink);

        return venueOrganizationLinkToVenueOrganizationLinkData(
                venueOrganizationLinkRepository.getByVenueIdAndOrganizationId(
                        creationData.venueId(), creationData.organizationId()));
    }

    public boolean isVenueAssignedToOrganization(@NonNull Long venueId, @NonNull Long organizationId) {
        return venueOrganizationLinkRepository.existsByVenueIdAndOrganizationId(venueId, organizationId);
    }

    @NonNull
    private VenueOrganizationLinkData venueOrganizationLinkToVenueOrganizationLinkData(
            @NonNull VenueOrganizationLink venueOrganizationLink) {
        return new VenueOrganizationLinkData(venueOrganizationLink.getVenueId(),
                venueOrganizationLink.getOrganizationId());
    }
}
