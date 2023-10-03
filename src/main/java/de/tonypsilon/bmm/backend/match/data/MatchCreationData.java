package de.tonypsilon.bmm.backend.match.data;

public record MatchCreationData(Long matchdayId,
                                Long homeTeamId,
                                Long awayTeamId,
                                String date,
                                Long refereeId) {
}
