package de.tonypsilon.bmm.backend.game.data;

import de.tonypsilon.bmm.backend.game.service.Result;

public record GameCreationData(Long matchId,
                               Integer boardNumber,
                               Long homeParticipantId,
                               Long awayParticipantId,
                               Result playedResultHome,
                               Result overruledResultHome,
                               Result playedResultAway,
                               Result overruledResultAway) {
}
