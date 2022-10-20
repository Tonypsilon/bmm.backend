package de.tonypsilon.bmm.backend.team.data;

import java.util.Optional;

public record TeamData(Long id,
                       Long organizationId,
                       Integer number,
                       Optional<Long> divisionId) {
}
