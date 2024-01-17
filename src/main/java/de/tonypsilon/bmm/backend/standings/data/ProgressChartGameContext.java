package de.tonypsilon.bmm.backend.standings.data;

import de.tonypsilon.bmm.backend.game.data.GameData;
import org.springframework.lang.NonNull;

public record ProgressChartGameContext(@NonNull GameData gameData,
                                       @NonNull Long homeTeamId,
                                       @NonNull Long awayTeamId) {}