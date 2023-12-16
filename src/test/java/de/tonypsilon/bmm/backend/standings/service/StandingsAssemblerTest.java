package de.tonypsilon.bmm.backend.standings.service;

import de.tonypsilon.bmm.backend.datatypes.IdAndLabel;
import de.tonypsilon.bmm.backend.division.data.DivisionData;
import de.tonypsilon.bmm.backend.division.service.DivisionService;
import de.tonypsilon.bmm.backend.exception.NotFoundException;
import de.tonypsilon.bmm.backend.game.data.GameData;
import de.tonypsilon.bmm.backend.game.service.GameService;
import de.tonypsilon.bmm.backend.game.service.Result;
import de.tonypsilon.bmm.backend.match.data.MatchData;
import de.tonypsilon.bmm.backend.match.data.MatchState;
import de.tonypsilon.bmm.backend.match.service.MatchService;
import de.tonypsilon.bmm.backend.standings.data.StandingsRowData;
import de.tonypsilon.bmm.backend.team.data.TeamData;
import de.tonypsilon.bmm.backend.team.data.TeamDivisionLinkData;
import de.tonypsilon.bmm.backend.team.service.TeamDivisionLinkService;
import de.tonypsilon.bmm.backend.team.service.TeamService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Optional;
import java.util.Set;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatExceptionOfType;
import static org.mockito.Mockito.*;

class StandingsAssemblerTest {

    private StandingsAssembler standingsAssembler;
    private final TeamDivisionLinkService teamDivisionLinkService = mock(TeamDivisionLinkService.class);
    private final TeamService teamService =  mock(TeamService.class);
    private final DivisionService divisionService = mock(DivisionService.class);
    private final MatchService matchService = mock(MatchService.class);
    private final GameService gameService = mock(GameService.class);
    private final Long divisionId = 1L;
    private final Long matchId = 2L;
    private final Long team1Id = 11L;
    private final Long team2Id = 12L;
    private final DivisionData divisionData = new DivisionData(divisionId, "Division 1", 1, 2, 1L, 2);
    private final TeamData team1Data = new TeamData(team1Id, 2L, 3, 1L, "ZChess Friends 3", "captain");
    private final TeamData team2Data = new TeamData(team2Id, 3L, 1, 2L, "Test Team", "captain2");

    @BeforeEach
    public void setUp() {
        this.standingsAssembler = new StandingsAssembler(
                teamDivisionLinkService,
                teamService,
                divisionService,
                matchService,
                gameService
        );
    }

    @Test
    void testAssembleStandingsDivisionDoesNotExist() {
        doThrow(new NotFoundException("Es gibt keine Staffel mit der ID 1!"))
                .when(divisionService).getDivisionDataById(divisionId);
        assertThatExceptionOfType(NotFoundException.class)
                .isThrownBy(() -> standingsAssembler.assembleStandings(divisionId))
                .withMessage("Es gibt keine Staffel mit der ID 1!");
    }

    @Test
    void testAssembleStandingsTwoTeamsNoMatchesPlayed() {
        when(divisionService.getDivisionDataById(divisionId)).thenReturn(divisionData);
        when(teamDivisionLinkService.getByDivisionId(divisionId))
                .thenReturn(Set.of(new TeamDivisionLinkData(team1Id, divisionId, 1),
                        new TeamDivisionLinkData(team2Id, divisionId, 2)));
        when(teamService.getTeamDataById(team1Id)).thenReturn(team1Data);
        when(teamService.getTeamDataById(team2Id)).thenReturn(team2Data);
        when(matchService.findByDivision(divisionId)).thenReturn(Set.of(new MatchData(
                matchId,
                1L,
                team1Id,
                team2Id,
                Optional.empty(),
                Optional.empty(),
                Optional.empty(),
                Optional.empty(),
                Optional.empty(),
                MatchState.OPEN)));

        var result = standingsAssembler.assembleStandings(divisionId);
        assertThat(result.rows()).hasSize(2);
        assertThat(result.rows()).containsExactly(
                new StandingsRowData(new IdAndLabel(team2Id, team2Data.name()),
                        List.of(new IdAndLabel(-1L, "X"), new IdAndLabel(matchId, " ")),
                        "0", "0"),
                new StandingsRowData(new IdAndLabel(team1Id, team1Data.name()),
                        List.of(new IdAndLabel(matchId, " "), new IdAndLabel(-1L, "X")),
                        "0", "0")
        );
    }

    @Test
    void testAssembleStandingsOneMatchPlayed() {
        when(divisionService.getDivisionDataById(divisionId)).thenReturn(divisionData);
        when(teamDivisionLinkService.getByDivisionId(divisionId))
                .thenReturn(Set.of(new TeamDivisionLinkData(team1Id, divisionId, 1),
                        new TeamDivisionLinkData(team2Id, divisionId, 2)));
        when(teamService.getTeamDataById(team1Id)).thenReturn(team1Data);
        when(teamService.getTeamDataById(team2Id)).thenReturn(team2Data);
        when(matchService.findByDivision(divisionId)).thenReturn(Set.of(new MatchData(
                matchId,
                1L,
                team1Id,
                team2Id,
                Optional.empty(),
                Optional.empty(),
                Optional.empty(),
                Optional.empty(),
                Optional.empty(),
                MatchState.CLOSED)));
        when(gameService.getByMatchId(matchId)).thenReturn(List.of(
                new GameData(1L, matchId, 1, 1L, 2L, Optional.of(Result.DRAW), Optional.empty(), Optional.of(Result.DRAW), Optional.empty()),
                new GameData(2L, matchId, 2, 3L, 4L, Optional.of(Result.LOSS), Optional.empty(), Optional.of(Result.WIN), Optional.empty())
        ));

        var result = standingsAssembler.assembleStandings(divisionId);
        assertThat(result.rows()).hasSize(2);
        assertThat(result.rows()).containsExactly(
                new StandingsRowData(new IdAndLabel(team2Id, team2Data.name()),
                        List.of(new IdAndLabel(-1L, "X"), new IdAndLabel(matchId, "1,5")),
                        "2", "1,5"),
                new StandingsRowData(new IdAndLabel(team1Id, team1Data.name()),
                        List.of(new IdAndLabel(matchId, "0,5"), new IdAndLabel(-1L, "X")),
                        "0", "0,5")
        );
    }

    @Test
    void testAssembleStandingsForfeitMatch() {
        when(divisionService.getDivisionDataById(divisionId)).thenReturn(divisionData);
        when(teamDivisionLinkService.getByDivisionId(divisionId))
                .thenReturn(Set.of(new TeamDivisionLinkData(team1Id, divisionId, 1),
                        new TeamDivisionLinkData(team2Id, divisionId, 2)));
        when(teamService.getTeamDataById(team1Id)).thenReturn(team1Data);
        when(teamService.getTeamDataById(team2Id)).thenReturn(team2Data);
        when(matchService.findByDivision(divisionId)).thenReturn(Set.of(new MatchData(
                matchId,
                1L,
                team1Id,
                team2Id,
                Optional.empty(),
                Optional.empty(),
                Optional.empty(),
                Optional.empty(),
                Optional.empty(),
                MatchState.WIN_HOME_BY_FORFEIT)));

        var result = standingsAssembler.assembleStandings(divisionId);
        assertThat(result.rows()).hasSize(2);
        assertThat(result.rows()).containsExactly(
                new StandingsRowData(new IdAndLabel(team1Id, team1Data.name()),
                        List.of(new IdAndLabel(-1L, "X"), new IdAndLabel(matchId, "+")),
                        "2", "1,5"),
                new StandingsRowData(new IdAndLabel(team2Id, team2Data.name()),
                        List.of(new IdAndLabel(matchId, "-"), new IdAndLabel(-1L, "X")),
                        "0", "0")
        );
    }
}