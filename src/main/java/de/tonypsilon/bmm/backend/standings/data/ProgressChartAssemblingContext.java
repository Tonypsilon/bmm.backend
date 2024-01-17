package de.tonypsilon.bmm.backend.standings.data;

import de.tonypsilon.bmm.backend.datatypes.IdAndLabel;
import de.tonypsilon.bmm.backend.game.data.GameData;
import de.tonypsilon.bmm.backend.game.service.Result;
import org.springframework.lang.NonNull;

import java.util.Collection;
import java.util.Map;
import java.util.Objects;

public class ProgressChartAssemblingContext {
    private final Integer numberOfRounds;
    private final Map<Integer, Collection<ProgressChartGameContext>> gamesByRound;

    public ProgressChartAssemblingContext(@NonNull Integer numberOfRounds,
                                          @NonNull Map<Integer, Collection<ProgressChartGameContext>> gamesByRound) {
        this.numberOfRounds = numberOfRounds;
        this.gamesByRound = gamesByRound;
    }

    public IdAndLabel getGame(@NonNull Long participantId, @NonNull Long teamId, int round) {
        return gamesByRound.get(round).stream()
                .filter(gameContext ->
                        gameContext.gameData().homeParticipantId().equals(participantId) && gameContext.homeTeamId().equals(teamId)
                        || gameContext.gameData().awayParticipantId().equals(participantId) && gameContext.awayTeamId().equals(teamId))
                .findAny()
                .map(gameContext -> this.gameToIdAndLabel(gameContext.gameData(), participantId))
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

    @NonNull
    public Integer getNumberOfRounds() {
        return this.numberOfRounds;
    }
}
