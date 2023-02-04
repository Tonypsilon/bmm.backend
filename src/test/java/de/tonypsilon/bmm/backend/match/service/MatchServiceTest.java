package de.tonypsilon.bmm.backend.match.service;

import de.tonypsilon.bmm.backend.division.service.DivisionService;
import de.tonypsilon.bmm.backend.exception.AlreadyExistsException;
import de.tonypsilon.bmm.backend.exception.BadDataException;
import de.tonypsilon.bmm.backend.exception.NotFoundException;
import de.tonypsilon.bmm.backend.match.data.CreateMatchData;
import de.tonypsilon.bmm.backend.match.data.Match;
import de.tonypsilon.bmm.backend.match.data.MatchData;
import de.tonypsilon.bmm.backend.match.data.MatchRepository;
import de.tonypsilon.bmm.backend.matchday.data.MatchdayData;
import de.tonypsilon.bmm.backend.matchday.service.MatchdayService;
import de.tonypsilon.bmm.backend.referee.data.RefereeData;
import de.tonypsilon.bmm.backend.referee.service.RefereeService;
import de.tonypsilon.bmm.backend.team.data.TeamData;
import de.tonypsilon.bmm.backend.team.service.TeamDivisionAssignmentService;
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
    private final TeamDivisionAssignmentService teamDivisionAssignmentService = 
    		mock(TeamDivisionAssignmentService.class);
    private final ValidationService validationService = new ValidationService();
    private final MatchData matchData1 = new MatchData(1L, 1L, 1L, 2L, Optional.of("1.1.2001"),
            2, 0, Optional.empty(), Optional.empty(), Optional.of(1L), Boolean.TRUE);
    private final MatchdayData matchdayData = new MatchdayData(1L, 2L, "1.1.2000", 3);
    private Match match1;

    @BeforeEach
    void setUp() {
        matchService = new MatchService(matchRepository,
                matchdayService,
                teamService,
                refereeService,
                divisionService,
                teamDivisionAssignmentService,
                validationService);
        match1 = new Match();
        match1.setId(1L);
        match1.setMatchdayId(1L);
        match1.setHomeTeamId(1L);
        match1.setAwayTeamId(2L);
        match1.setDate("1.1.2001");
        match1.setHomeTeamPoints(2);
        match1.setAwayTeamPoints(0);
        match1.setOverruledHomeBoardHalfPoints(null);
        match1.setOverruledAwayBoardHalfPoints(null);
        match1.setRefereeId(1L);
        match1.setEditable(Boolean.TRUE);
    }

    @Test
    void testCreateMatchOk() {
        when(matchdayService.getMatchdayDataById(1L)).thenReturn(matchdayData);
        when(teamService.getTeamDataById(1L)).thenReturn(new TeamData(1L, 1L, 1));
        when(teamService.getTeamDataById(2L)).thenReturn(new TeamData(2L, 2L, 2));
        when(matchRepository.existsByMatchdayIdAndHomeTeamIdOrAwayTeamId(1L, 1L)).thenReturn(Boolean.FALSE);
        when(matchRepository.existsByMatchdayIdAndHomeTeamIdOrAwayTeamId(1L, 2L)).thenReturn(Boolean.FALSE);
        when(divisionService.getSeasonIdByDivisionId(2L)).thenReturn(1L);
        when(refereeService.findById(1L)).thenReturn(Optional.of(new RefereeData(1L, 1L, "Ref", "eree", "ref@eree.com")));
        when(matchRepository.getByMatchdayIdAndHomeTeamIdAndAwayTeamId(1L, 1L, 2L)).thenReturn(match1);
        CreateMatchData createMatchData = new CreateMatchData(1L, 1L, 2L, Optional.of("1.1.2001"), Optional.of(1L));
        MatchData actual = matchService.createMatch(createMatchData);
        assertEquals(matchData1, actual);
    }

    @Test
    void testCreateMatchHomeTeamDoesNotExist() {
        when(matchdayService.getMatchdayDataById(1L)).thenReturn(matchdayData);
        when(teamService.getTeamDataById(-1L)).thenThrow(new NotFoundException("Es gibt kein Team mit ID -1!"));
        CreateMatchData createMatchData = new CreateMatchData(1L, -1L, 2L,
                Optional.empty(), Optional.empty());
        NotFoundException actualException = assertThrows(NotFoundException.class,
                () -> matchService.createMatch(createMatchData));
        assertEquals("Es gibt kein Team mit ID -1!", actualException.getMessage());
    }

    @Test
    void testCreateMatchAwayTeamDoesNotExist() {
        when(matchdayService.getMatchdayDataById(1L)).thenReturn(matchdayData);
        when(teamService.getTeamDataById(1L)).thenReturn(new TeamData(1L, 1L, 1));
        when(teamService.getTeamDataById(-1L)).thenThrow(new NotFoundException("Es gibt kein Team mit ID -1!"));
        CreateMatchData createMatchData = new CreateMatchData(1L, 1L, -1L,
                Optional.empty(), Optional.empty());
        NotFoundException actualException = assertThrows(NotFoundException.class,
                () -> matchService.createMatch(createMatchData));
        assertEquals("Es gibt kein Team mit ID -1!", actualException.getMessage());
    }

    @Test
    void testCreateMatchWrongDivision() {
        when(matchdayService.getMatchdayDataById(1L)).thenReturn(matchdayData);
        when(teamService.getTeamDataById(1L)).thenReturn(new TeamData(1L, 1L, 1));
        when(teamService.getTeamDataById(2L)).thenReturn(new TeamData(2L, 2L, 2));
        when(teamDivisionAssignmentService.getDivisionIdOfTeam(1L)).thenReturn(2L);
        when(teamDivisionAssignmentService.getDivisionIdOfTeam(2L)).thenReturn(5L);
        CreateMatchData createMatchData = new CreateMatchData(1L, 1L, 2L,
                Optional.empty(), Optional.empty());
        BadDataException actualException = assertThrows(BadDataException.class,
                () -> matchService.createMatch(createMatchData));
        assertEquals("Mindestens eine der beiden Mannschaften gehört nicht zur richtigen Staffel!",
                actualException.getMessage());
    }

    @Test
    void testCreateMatchHomeTeamAlreadyHasMatch() {
        when(matchdayService.getMatchdayDataById(1L)).thenReturn(matchdayData);
        when(teamService.getTeamDataById(1L)).thenReturn(new TeamData(1L, 1L, 1));
        when(teamService.getTeamDataById(2L)).thenReturn(new TeamData(2L, 2L, 2));
        when(matchRepository.existsByMatchdayIdAndHomeTeamIdOrAwayTeamId(1L, 1L)).thenReturn(Boolean.TRUE);
        CreateMatchData createMatchData = new CreateMatchData(1L, 1L, 2L,
                Optional.empty(), Optional.empty());
        AlreadyExistsException actualException = assertThrows(AlreadyExistsException.class,
                () -> matchService.createMatch(createMatchData));
        assertEquals("Die Heimmannschaft hat an diesem Spieltag schon einen Wettkampf!",
                actualException.getMessage());
    }

    @Test
    void testCreateMatchAwayTeamAlreadyHasMatch() {
        when(matchdayService.getMatchdayDataById(1L)).thenReturn(matchdayData);
        when(teamService.getTeamDataById(1L)).thenReturn(new TeamData(1L, 1L, 1));
        when(teamService.getTeamDataById(2L)).thenReturn(new TeamData(2L, 2L, 2));
        when(matchRepository.existsByMatchdayIdAndHomeTeamIdOrAwayTeamId(1L, 1L)).thenReturn(Boolean.FALSE);
        when(matchRepository.existsByMatchdayIdAndHomeTeamIdOrAwayTeamId(1L, 2L)).thenReturn(Boolean.TRUE);
        CreateMatchData createMatchData = new CreateMatchData(1L, 1L, 2L,
                Optional.empty(), Optional.empty());
        AlreadyExistsException actualException = assertThrows(AlreadyExistsException.class,
                () -> matchService.createMatch(createMatchData));
        assertEquals("Die Gastmannschaft hat an diesem Spieltag schon einen Wettkampf!",
                actualException.getMessage());
    }

    @Test
    void testCreateMatchInvalidDateString() {
        when(matchdayService.getMatchdayDataById(1L)).thenReturn(matchdayData);
        when(teamService.getTeamDataById(1L)).thenReturn(new TeamData(1L, 1L, 1));
        when(teamService.getTeamDataById(2L)).thenReturn(new TeamData(2L, 2L, 2));
        when(matchRepository.existsByMatchdayIdAndHomeTeamIdOrAwayTeamId(1L, 1L)).thenReturn(Boolean.FALSE);
        when(matchRepository.existsByMatchdayIdAndHomeTeamIdOrAwayTeamId(1L, 2L)).thenReturn(Boolean.FALSE);
        CreateMatchData createMatchData = new CreateMatchData(1L, 1L, 2L, Optional.of("1.1%2001"), Optional.of(1L));
        BadDataException actualException = assertThrows(BadDataException.class,
                () -> matchService.createMatch(createMatchData));
        assertEquals("Das Datum enthält ungültige Zeichen!", actualException.getMessage());
    }

    @Test
    void testCreateMatchRefereeDoesNotExist() {
        when(matchdayService.getMatchdayDataById(1L)).thenReturn(matchdayData);
        when(teamService.getTeamDataById(1L)).thenReturn(new TeamData(1L, 1L, 1));
        when(teamService.getTeamDataById(2L)).thenReturn(new TeamData(2L, 2L, 2));
        when(matchRepository.existsByMatchdayIdAndHomeTeamIdOrAwayTeamId(1L, 1L)).thenReturn(Boolean.FALSE);
        when(matchRepository.existsByMatchdayIdAndHomeTeamIdOrAwayTeamId(1L, 2L)).thenReturn(Boolean.FALSE);
        when(refereeService.findById(-1L)).thenReturn(Optional.empty());
        CreateMatchData createMatchData = new CreateMatchData(1L, 1L, 2L, Optional.of("1.1.2001"), Optional.of(-1L));
        NotFoundException actualException = assertThrows(NotFoundException.class,
                () -> matchService.createMatch(createMatchData));
        assertEquals("Es gibt keinen Schiedsrichter mit der ID -1!", actualException.getMessage());

    }

    @Test
    void testCreateMatchRefereeNotValidForSeason() {
        when(matchdayService.getMatchdayDataById(1L)).thenReturn(matchdayData);
        when(teamService.getTeamDataById(1L)).thenReturn(new TeamData(1L, 1L, 1));
        when(teamService.getTeamDataById(2L)).thenReturn(new TeamData(2L, 2L, 2));
        when(matchRepository.existsByMatchdayIdAndHomeTeamIdOrAwayTeamId(1L, 1L)).thenReturn(Boolean.FALSE);
        when(matchRepository.existsByMatchdayIdAndHomeTeamIdOrAwayTeamId(1L, 2L)).thenReturn(Boolean.FALSE);
        when(divisionService.getSeasonIdByDivisionId(2L)).thenReturn(2L);
        when(refereeService.findById(1L)).thenReturn(Optional.of(new RefereeData(1L, 1L, "Ref", "eree", "ref@eree.com")));
        CreateMatchData createMatchData = new CreateMatchData(1L, 1L, 2L, Optional.of("1.1.2001"), Optional.of(1L));
        BadDataException actualException = assertThrows(BadDataException.class,
                () -> matchService.createMatch(createMatchData));
        assertEquals("Der Schiedsrichter mit der ID 1 passt nicht zur Saison mit der ID 2!", actualException.getMessage());
    }

}