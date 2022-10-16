package de.tonypsilon.bmm.backend.match.service;

import de.tonypsilon.bmm.backend.division.service.DivisionService;
import de.tonypsilon.bmm.backend.match.data.Match;
import de.tonypsilon.bmm.backend.match.data.MatchData;
import de.tonypsilon.bmm.backend.match.data.MatchRepository;
import de.tonypsilon.bmm.backend.matchday.data.MatchdayRepository;
import de.tonypsilon.bmm.backend.matchday.service.MatchdayService;
import de.tonypsilon.bmm.backend.referee.service.RefereeService;
import de.tonypsilon.bmm.backend.team.service.TeamService;
import de.tonypsilon.bmm.backend.validation.service.ValidationService;
import org.junit.jupiter.api.BeforeEach;

import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.mock;

class MatchServiceTest {

    private MatchService matchService;
    private final MatchRepository matchRepository = mock(MatchdayRepository.class);
    private final MatchdayService matchdayService = mock(MatchdayService.class);
    private final TeamService teamService = mock(TeamService.class);
    private final RefereeService refereeService = mock(RefereeService.class);
    private final DivisionService divisionService = mock(DivisionService.class);
    private final ValidationService validationService = new ValidationService();
    private final MatchData matchData1 = new MatchData(1L, 1L, 1L, 2L, Optional.empty(),
            2, 0, Optional.empty(), Optional.empty(), Optional.of(1L));
    private Match match1;

    @BeforeEach
    private void setUp() {
        matchService = new MatchService(matchRepository,
                matchdayService,
                teamService,
                refereeService,
                divisionService,
                validationService);
        match1 = new Match();
        match1.setId(1L);
        match1.setMatchdayId(1L);
        match1.setHomeTeamId(1L);
        match1.setAwayTeamId(2L);
        match1.setDate(null);
        match1.setHomeTeamPoints(2);
        match1.setAwayTeamPoints(0);
        match1.setOverruledHomeBoardHalfPoints(null);
        match1.setOverruledAwayBoardHalfPoints(null);
        match1.setRefereeId(1L);
    }

}