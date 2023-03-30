package de.tonypsilon.bmm.backend.match.data;

import java.util.Optional;

public record MatchCreationData(Long matchdayId,
                                Long homeTeamId,
                                Long awayTeamId,
                                Optional<String> date,
                                Optional<Long> refereeId) {
}
