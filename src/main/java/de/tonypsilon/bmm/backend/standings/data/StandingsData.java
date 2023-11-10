package de.tonypsilon.bmm.backend.standings.data;

import java.util.List;

public record StandingsData(Integer numberOfTeams, List<StandingsRowData> rows) {
}
