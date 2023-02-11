package de.tonypsilon.bmm.backend.club.data;

import java.util.Optional;

public record VenueCreationData(Long clubId, String address, Optional<String> hints) {
}
