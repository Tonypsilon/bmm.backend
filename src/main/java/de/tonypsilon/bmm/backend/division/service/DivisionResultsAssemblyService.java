package de.tonypsilon.bmm.backend.division.service;

import de.tonypsilon.bmm.backend.division.data.DivisionResultsData;
import de.tonypsilon.bmm.backend.game.service.GameService;
import de.tonypsilon.bmm.backend.match.service.MatchService;
import de.tonypsilon.bmm.backend.matchday.service.MatchdayService;
import de.tonypsilon.bmm.backend.referee.service.RefereeService;
import de.tonypsilon.bmm.backend.season.service.PlayingDateService;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
public class DivisionResultsAssemblyService {

    private final DivisionService divisionService;
    private final MatchdayService matchdayService;
    private final MatchService matchService;
    private final RefereeService refereeService;
    private final PlayingDateService playingDateService;
    private final GameService gameService;

    public DivisionResultsAssemblyService(
            final DivisionService divisionService,
            final MatchdayService matchdayService,
            final MatchService matchService,
            final RefereeService refereeService,
            final PlayingDateService playingDateService,
            final GameService gameService) {
        this.divisionService = divisionService;
        this.matchdayService = matchdayService;
        this.matchService = matchService;
        this.refereeService = refereeService;
        this.playingDateService = playingDateService;
        this.gameService = gameService;
    }

    @NonNull
    public DivisionResultsData assembleDivisionResults(@NonNull Long divisionId) {
        return new DivisionResultsData(List.of());
    }
}
