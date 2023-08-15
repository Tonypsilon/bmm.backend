package de.tonypsilon.bmm.backend.venue.data;

import javax.annotation.Nullable;

public record VenueData(Long id,
                        Long clubId,
                        String address,
                        @Nullable String hints) {
}
