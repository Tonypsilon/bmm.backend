package de.tonypsilon.bmm.backend.standings.data;

import de.tonypsilon.bmm.backend.datatypes.IdAndLabel;
import de.tonypsilon.bmm.backend.game.data.GameData;
import de.tonypsilon.bmm.backend.game.service.Result;
import org.springframework.lang.NonNull;

import java.util.Collection;
import java.util.Map;
import java.util.Objects;

public class ProgressChartAssemblingContext {
    private final Long divisionId;
    private final Map<Integer, Collection<GameData>> gamesByRound;

    public ProgressChartAssemblingContext(@NonNull Long divisionId,
                                          @NonNull Map<Integer, Collection<GameData>> gamesByRound) {
        this.divisionId = divisionId;
        this.gamesByRound = gamesByRound;
    }

    public IdAndLabel getGame(@NonNull Long participantId, int round) {
        return gamesByRound.get(round).stream()
                .filter(gameData -> gameData.homeParticipantId().equals(participantId)
                        || gameData.awayParticipantId().equals(participantId))
                .findAny()
                .map(gameData -> this.gameToIdAndLabel(gameData, participantId))
                .orElse(new IdAndLabel(-1L, " "));
    }

    private IdAndLabel gameToIdAndLabel(@NonNull GameData gameData, @NonNull Long participantId) {
        if(!Objects.equals(gameData.homeParticipantId(), (participantId))
                && !Objects.equals(gameData.awayParticipantId(), participantId)) {
            throw new IllegalStateException("Given participant must match game!");
        }
        if(gameData.homeParticipantId().equals(participantId)) {
            return new IdAndLabel(
                    gameData.id(),
                    gameData.overruledResultHome().map(Result::getLabel)
                            .or(() -> gameData.playedResultHome().map(Result::getLabel))
                            .orElse(" ")
            );
        }
        // gameData.awayParticipantId() equals participantId
        return new IdAndLabel(
                gameData.id(),
                gameData.overruledResultAway().map(Result::getLabel)
                        .or(() -> gameData.playedResultAway().map(Result::getLabel))
                        .orElse(" ")
        );
    }
}
