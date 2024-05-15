package de.tonypsilon.bmm.backend.dwz.data;

import de.tonypsilon.bmm.backend.game.data.GameData;
import de.tonypsilon.bmm.backend.game.service.Result;
import org.springframework.lang.NonNull;

import java.util.Comparator;
import java.util.Map;
import java.util.Objects;

public class DwzAssemblingContext {
    private final int numberOfRounds;
    private final Map<Long, Map<Integer, GameData>> gamesByRoundByParticipant;
    private final Map<Long, Integer> participantOrdinal;

    public DwzAssemblingContext(int numberOfRounds,
                                @NonNull final Map<Long, Map<Integer, GameData>> gamesByRoundByParticipant,
                                @NonNull final Map<Long, Integer> participantOrdinal) {
        this.numberOfRounds = numberOfRounds;
        this.gamesByRoundByParticipant = gamesByRoundByParticipant;
        this.participantOrdinal = participantOrdinal;
    }

    public int getNumberOfRounds() {
        return numberOfRounds;
    }

    @NonNull
    public String getGamesByParticipant(@NonNull Long participantId) {
        Objects.requireNonNull(participantId);
        final Map<Integer, GameData> gamesByRound = gamesByRoundByParticipant.get(participantId);
        Objects.requireNonNull(gamesByRound);
        final StringBuilder result = new StringBuilder();
        for (int i = 1; i <= numberOfRounds; i++) {
            GameData gameData = gamesByRound.get(i);
            if (gameData == null) {
                result.append(" ".repeat(5));
            } else {
                boolean isHome = gameData.homeParticipantId().equals(participantId);
                Long opponentId = isHome ?
                        gameData.awayParticipantId() : gameData.homeParticipantId();
                Result gameResult = isHome ?
                        gameData.playedResultHome().orElse(null) :
                        gameData.playedResultAway().orElse(null);
                if (gameResult == null || gameResult == Result.WIN_FORFEIT || gameResult == Result.LOSS_FORFEIT) {
                    result.append(" ".repeat(5));
                } else {
                    result.append(toResultChar(gameResult));
                    result.append(getColorByBoard(gameData.boardNumber(), isHome));
                    if(participantOrdinal.get(opponentId) == null) {
                        System.out.println(participantId + " " + opponentId + gameData);
                        for (Map.Entry<Long, Integer> entry : participantOrdinal.entrySet().stream().sorted(Comparator.comparingLong(Map.Entry::getKey)).toList()) {
                            System.out.println(entry.getKey() + " " + entry.getValue());
                        }
                    }
                    result.append(opponentOrdinalToString(participantOrdinal.get(opponentId)));
                }
            }
            result.append(" ");
        }
        return result.toString();
    }

    private char toResultChar(Result result) {
        return switch (result) {
            case WIN -> '1';
            case DRAW -> 'R';
            case LOSS -> '0';
            case WIN_FORFEIT,LOSS_FORFEIT -> ' ';
        };
    }

    private char getColorByBoard(int boardNumber, boolean isHome) {
        return isHome ?
                boardNumber % 2 == 0 ? 'W' : 'B'
                : boardNumber % 2 == 0 ? 'B' : 'W';
    }

    private String opponentOrdinalToString(Integer opponentOrdinal) {
        Objects.requireNonNull(opponentOrdinal);
        if (opponentOrdinal < 0) {
            throw new IllegalArgumentException("OpponentOrdinal must be non-negative");
        }
        if (opponentOrdinal < 10) {
            return "  " + opponentOrdinal;
        }
        if (opponentOrdinal < 100) {
            return " " + opponentOrdinal;
        }
        if (opponentOrdinal < 1000) {
            return String.valueOf(opponentOrdinal);
        }
        throw new IllegalArgumentException("OpponentOrdinal must be less than 1000");
    }

}
