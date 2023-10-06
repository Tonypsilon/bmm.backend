package de.tonypsilon.bmm.backend.matchday.data;

import de.tonypsilon.bmm.backend.match.data.MatchResultClientData;

import java.util.List;

public record MatchdayClientData(String date,
                                 Integer round,
                                 List<MatchResultClientData> matches) {
}
