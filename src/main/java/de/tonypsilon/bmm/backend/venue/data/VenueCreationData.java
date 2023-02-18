package de.tonypsilon.bmm.backend.venue.data;

import org.springframework.lang.Nullable;

import java.util.Optional;

public record VenueCreationData(Long clubId, String address, @Nullable String hints) {
}
