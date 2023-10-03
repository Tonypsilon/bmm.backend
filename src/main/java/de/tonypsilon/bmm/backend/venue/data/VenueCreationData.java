package de.tonypsilon.bmm.backend.venue.data;

import org.springframework.lang.Nullable;

public record VenueCreationData(Long clubId, String address, @Nullable String hints) {
}
