package de.tonypsilon.bmm.backend.matchday.data;

public record MatchdayData(Long id,
                           Long divisionId,
                           String date,
                           Integer round) {
}
