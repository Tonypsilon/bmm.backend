package de.tonypsilon.bmm.backend.standings.data;

import de.tonypsilon.bmm.backend.datatypes.IdAndLabel;
import de.tonypsilon.bmm.backend.match.data.ParticipantDataForClient;

import java.util.List;

public record ParticipantProgressChartData(ParticipantDataForClient participant, List<IdAndLabel> results) {
}
