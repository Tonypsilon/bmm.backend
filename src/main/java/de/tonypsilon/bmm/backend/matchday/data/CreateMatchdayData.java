package de.tonypsilon.bmm.backend.matchday.data;

public record CreateMatchdayData(Long divisionId,
                                 String date,
                                 Integer round) {
}
