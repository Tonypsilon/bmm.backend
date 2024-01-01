package de.tonypsilon.bmm.backend.standings.data;

import de.tonypsilon.bmm.backend.datatypes.IdAndLabel;

import java.util.List;

public record TeamProgressChartData(IdAndLabel team, List<ParticipantProgressChartData> participantProgressCharts) {
}
