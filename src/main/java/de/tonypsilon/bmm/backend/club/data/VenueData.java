package de.tonypsilon.bmm.backend.club.data;

import java.util.Optional;

public record VenueData(Long id, Long clubId, String address, Optional<String> hints) {
}
