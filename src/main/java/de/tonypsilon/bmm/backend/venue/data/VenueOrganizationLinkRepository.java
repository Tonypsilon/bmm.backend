package de.tonypsilon.bmm.backend.venue.data;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Repository;

@Repository
public interface VenueOrganizationLinkRepository
        extends JpaRepository<VenueOrganizationLink, VenueOrganizationLinkKey> {

    boolean existsByVenueIdAndOrganizationId(@NonNull Long venueId, @NonNull Long organizationId);

    VenueOrganizationLink getByVenueIdAndOrganizationId(
            @NonNull Long venueId, @NonNull Long organizationId);
}
