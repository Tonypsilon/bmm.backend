package de.tonypsilon.bmm.backend.venue.data;

import org.springframework.lang.NonNull;

import javax.persistence.*;

@Entity
@IdClass(VenueOrganizationLinkKey.class)
@Table(name = "venueorganizationlink")
public class VenueOrganizationLink {

    @Id
    @Column(name = "venue_id", nullable = false)
    private Long venueId;

    @Id
    @Column(name = "organization_id", nullable = false)
    private Long organizationId;

    @NonNull
    public Long getVenueId() {
        return venueId;
    }

    public void setVenueId(@NonNull Long venueId) {
        this.venueId = venueId;
    }

    @NonNull
    public Long getOrganizationId() {
        return organizationId;
    }

    public void setOrganizationId(@NonNull Long organizationId) {
        this.organizationId = organizationId;
    }
}
