package de.tonypsilon.bmm.backend.division.data;

import de.tonypsilon.bmm.backend.matchday.data.MatchdayClientData;

import java.util.List;

public record DivisionResultsData(List<MatchdayClientData> matchdays) {
}
