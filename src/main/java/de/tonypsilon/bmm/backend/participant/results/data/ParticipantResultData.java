package de.tonypsilon.bmm.backend.participant.results.data;

import de.tonypsilon.bmm.backend.datatypes.IdAndLabel;

public record ParticipantResultData(
        int round,
        String color,
        IdAndLabel opponentTeam,
        int boardNumber,
        IdAndLabel opponent,
        String result
) {
}
