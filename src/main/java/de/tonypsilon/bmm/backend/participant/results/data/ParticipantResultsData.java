package de.tonypsilon.bmm.backend.participant.results.data;

import de.tonypsilon.bmm.backend.datatypes.IdAndLabel;

import java.util.List;

public record ParticipantResultsData(
        String name,
        String rating,
        IdAndLabel team,
        List<ParticipantResultData> results,
        String performance,
        String ratingChange,
        Long seasonId
) {
}
