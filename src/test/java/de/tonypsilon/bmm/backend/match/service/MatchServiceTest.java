package de.tonypsilon.bmm.backend.match.service;

import de.tonypsilon.bmm.backend.division.service.DivisionService;
import de.tonypsilon.bmm.backend.exception.NotFoundException;
import de.tonypsilon.bmm.backend.match.data.CreateMatchData;
import de.tonypsilon.bmm.backend.match.data.Match;
import de.tonypsilon.bmm.backend.match.data.MatchData;
import de.tonypsilon.bmm.backend.match.data.MatchRepository;
import de.tonypsilon.bmm.backend.matchday.data.MatchdayData;
import de.tonypsilon.bmm.backend.matchday.data.MatchdayRepository;
import de.tonypsilon.bmm.backend.matchday.service.MatchdayService;
import de.tonypsilon.bmm.backend.referee.service.RefereeService;
import de.tonypsilon.bmm.backend.team.service.TeamService;
import de.tonypsilon.bmm.backend.validation.service.ValidationService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class MatchServiceTest {

    private MatchService matchService;
    private final MatchRepository matchRepository = mock(MatchRepository.class);
    private final MatchdayService matchdayService = mock(MatchdayService.class);
    private final TeamService teamService = mock(TeamService.class);
    private final RefereeService refereeService = mock(RefereeService.class);
    private final DivisionService divisionService = mock(DivisionService.class);
    private final ValidationService validationService = new ValidationService();
    private final MatchData matchData1 = new MatchData(1L, 1L, 1L, 2L, Optional.empty(),
            2, 0, Optional.empty(), Optional.empty(), Optional.of(1L));
    private final MatchdayData matchdayData = new MatchdayData(1L, 2L, "1.1.2000", 3);
    private Match match1;

    @BeforeEach
    void setUp() {
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

    @Test
    void testCreateMatchOk() {
        when(matchdayService.findById(1L)).thenReturn(Optional.of(matchdayData));
        when(teamService.existsById(1L)).thenReturn(Boolean.TRUE);
        when(teamService.existsById(2L)).thenReturn(Boolean.TRUE);
    }

    @Test
    void testCreateMatchMatchdayDoesntExist() {
        when(matchdayService.findById(-1L)).thenReturn(Optional.empty());
        CreateMatchData createMatchData = new CreateMatchData(-1L, 1L, 2L,
                Optional.empty(), Optional.empty());
        NotFoundException actualException = assertThrows(NotFoundException.class,
                () -> matchService.createMatch(createMatchData));
        assertEquals("Es gibt keinen Spieltag mit der ID -1!", actualException.getMessage());
    }

    @Test
    void testCreateMatchHomeTeamDoesNotExist() {
        when(matchdayService.findById(1L)).thenReturn(Optional.of(matchdayData));
        when(teamService.existsById(-1L)).thenReturn(Boolean.FALSE);
        CreateMatchData createMatchData = new CreateMatchData(1L, -1L, 2L,
                Optional.empty(), Optional.empty());
        NotFoundException actualException = assertThrows(NotFoundException.class,
                () -> matchService.createMatch(createMatchData));
        assertEquals("Die Heimmannschaft mit ID -1 existiert nicht!", actualException.getMessage());
    }

    @Test
    void testCreateMatchAwayTeamDoesNotExist() {
        when(matchdayService.findById(1L)).thenReturn(Optional.of(matchdayData));
        when(teamService.existsById(1L)).thenReturn(Boolean.TRUE);
        when(teamService.existsById(-1L)).thenReturn(Boolean.FALSE);
        CreateMatchData createMatchData = new CreateMatchData(1L, 1L, -1L,
                Optional.empty(), Optional.empty());
        NotFoundException actualException = assertThrows(NotFoundException.class,
                () -> matchService.createMatch(createMatchData));
        assertEquals("Die Gastmannschaft mit ID -1 existiert nicht!", actualException.getMessage());
    }

    @Test
    void testCreateMatchHomeTeamAlreadyHasMatch() {

    }

    @Test
    void testCreateMatchAwayTeamAlreadyHasMatch() {

    }

}