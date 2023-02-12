package de.tonypsilon.bmm.backend.venue.data;

import java.io.Serializable;
import java.util.Objects;

public class VenueOrganizationLinkKey implements Serializable {

    private static final long serialVersionUID = 1L;

    private Long venueId;

    private Long organizationId;

    public Long getVenueId() {
        return venueId;
    }

    public void setVenueId(Long venueId) {
        this.venueId = venueId;
    }

    public Long getOrganizationId() {
        return organizationId;
    }

    public void setOrganizationId(Long organizationId) {
        this.organizationId = organizationId;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        VenueOrganizationLinkKey that = (VenueOrganizationLinkKey) o;
        return Objects.equals(venueId, that.venueId) && Objects.equals(organizationId, that.organizationId);
    }

    @Override
    public int hashCode() {
        return Objects.hash(venueId, organizationId);
    }
}
