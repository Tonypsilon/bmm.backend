package de.tonypsilon.bmm.backend.standings.data;

import java.util.List;

public record ProgressChartData(int numberOfRounds, List<TeamProgressChartData> teamProgressCharts) {
}
