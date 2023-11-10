package de.tonypsilon.bmm.backend.standings.data;

import de.tonypsilon.bmm.backend.datatypes.IdAndLabel;

import java.util.List;

public record StandingsRowData(IdAndLabel team, List<IdAndLabel> results, String teamPoints, String boardPoints) {
}
