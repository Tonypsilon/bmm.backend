package de.tonypsilon.bmm.backend.standings.service;

import de.tonypsilon.bmm.backend.datatypes.IdAndLabel;
import de.tonypsilon.bmm.backend.division.data.DivisionData;
import de.tonypsilon.bmm.backend.division.service.DivisionService;
import de.tonypsilon.bmm.backend.game.data.GameData;
import de.tonypsilon.bmm.backend.game.service.GameService;
import de.tonypsilon.bmm.backend.game.service.Result;
import de.tonypsilon.bmm.backend.match.data.MatchData;
import de.tonypsilon.bmm.backend.match.data.MatchState;
import de.tonypsilon.bmm.backend.match.service.MatchService;
import de.tonypsilon.bmm.backend.standings.data.*;
import de.tonypsilon.bmm.backend.team.data.TeamDivisionLinkData;
import de.tonypsilon.bmm.backend.team.service.TeamDivisionLinkService;
import de.tonypsilon.bmm.backend.team.service.TeamService;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Service;

import java.util.*;

@Service
public class StandingsAssembler {

    private final TeamDivisionLinkService teamDivisionLinkService;
    private final TeamService teamService;
    private final DivisionService divisionService;
    private final MatchService matchService;
    private final GameService gameService;
    private final String vsSelf = "X";

    public StandingsAssembler(
            final TeamDivisionLinkService teamDivisionLinkService,
            final TeamService teamService,
            final DivisionService divisionService,
            final MatchService matchService,
            final GameService gameService
            ) {
        this.teamDivisionLinkService = teamDivisionLinkService;
        this.teamService = teamService;
        this.divisionService = divisionService;
        this.matchService = matchService;
        this.gameService = gameService;
    }

    @NonNull
    public StandingsData assembleStandings(@NonNull Long divisionId) {
        DivisionData division = divisionService.getDivisionDataById(divisionId);
        final List<TeamStandings> teamsStandings = teamDivisionLinkService.getByDivisionId(divisionId).stream()
                .map(TeamDivisionLinkData::teamId)
                .map(teamService::getTeamDataById)
                .map(TeamStandings::new)
                .toList();
        final Collection<MatchData> results = matchService.findByDivision(divisionId);
        determineStandings(teamsStandings, results, division.numberOfBoards() + 1);
        final List<TeamStandings> sortedTeamsStandings = teamsStandings.stream()
                .sorted(Comparator.comparingInt(TeamStandings::getTeamPoints)
                        .thenComparingInt(TeamStandings::getDoubledBoardPoints)
                        .reversed()
                        .thenComparing(teamStandings -> teamStandings.getTeam().name()))
                .toList();
        final List<StandingsRowData> standingsRowData = getStandingsRowData(sortedTeamsStandings, results);
        return new StandingsData(standingsRowData);
    }

    private void determineStandings(List<TeamStandings> teamsStandings,
                                    Collection<MatchData> results,
                                    int doubleForfeitWinBoardPoints) {
        for (MatchData matchData : results) {
            switch (matchData.matchState()) {
                case WIN_HOME_BY_FORFEIT -> teamsStandings.stream()
                        .filter(teamStandings -> teamStandings.getTeam().id().equals(matchData.homeTeamId()))
                        .forEach(teamStandings -> teamStandings.addResult(2, doubleForfeitWinBoardPoints));
                case WIN_AWAY_BY_FORFEIT -> teamsStandings.stream()
                        .filter(teamStandings -> teamStandings.getTeam().id().equals(matchData.awayTeamId()))
                        .forEach(teamStandings -> teamStandings.addResult(2, doubleForfeitWinBoardPoints));
                case OPEN, IN_CLARIFICATION, CLOSED -> {
                    StandingsResultFromMatch standingsResultFromMatch = standingsResultFromMatch(matchData);
                    teamsStandings.stream()
                            .filter(teamStandings -> teamStandings.getTeam().id().equals(matchData.homeTeamId()))
                            .forEach(teamStandings -> teamStandings.addResult(
                                    standingsResultFromMatch.homeTeamPoints(),
                                    standingsResultFromMatch.doubledHomeBoardPoints()
                            ));
                    teamsStandings.stream()
                            .filter(teamStandings -> teamStandings.getTeam().id().equals(matchData.awayTeamId()))
                            .forEach(teamStandings -> teamStandings.addResult(
                                    standingsResultFromMatch.awayTeamPoints(),
                                    standingsResultFromMatch.doubledAwayBoardPoints()
                            ));
                }
            }
        }
    }

    private StandingsResultFromMatch standingsResultFromMatch(MatchData matchData) {
        if (Set.of(MatchState.WIN_HOME_BY_FORFEIT, MatchState.WIN_AWAY_BY_FORFEIT)
                .contains(matchData.matchState())) {
            throw new UnsupportedOperationException("Can't do this operation for forfeited matches!");
        }
        Integer doubledHomeBoardPoints = doubledHomeBoardPoints(matchData);
        Integer doubledAwayBoardPoints = doubledAwayBoardPoints(matchData);
        return new StandingsResultFromMatch(
                homeTeamPointsFromBoardPoints(doubledHomeBoardPoints, doubledAwayBoardPoints),
                doubledHomeBoardPoints,
                awayTeamPointsFromBoardPoints(doubledHomeBoardPoints, doubledAwayBoardPoints),
                doubledAwayBoardPoints
        );
    }

    private Integer doubledHomeBoardPoints(MatchData matchData) {
        Collection<GameData> gamesFromMatch = gameService.getByMatchId(matchData.id());
        return matchData.overruledHomeBoardHalfPoints().orElseGet(() ->
                gamesFromMatch.stream()
                        .mapToInt(gameData -> gameData.overruledResultHome().map(Result::getDoubledValue)
                                .or(() -> gameData.playedResultHome().map(Result::getDoubledValue))
                                .orElse(0))
                        .sum());
    }

    private int doubledAwayBoardPoints(MatchData matchData) {
        Collection<GameData> gamesFromMatch = gameService.getByMatchId(matchData.id());
        return matchData.overruledAwayBoardHalfPoints().orElseGet(() ->
                gamesFromMatch.stream()
                        .mapToInt(gameData -> gameData.overruledResultAway().map(Result::getDoubledValue)
                                .or(() -> gameData.playedResultAway().map(Result::getDoubledValue))
                                .orElse(0))
                        .sum());
    }

    private Integer homeTeamPointsFromBoardPoints(int doubledHomeBoardPoints, int doubledAwayBoardPoints) {
        if (doubledHomeBoardPoints == 0 && doubledAwayBoardPoints == 0) {
            return 0;
        }
        if (doubledHomeBoardPoints > doubledAwayBoardPoints) {
            return 2;
        }
        if (doubledAwayBoardPoints > doubledHomeBoardPoints) {
            return 0;
        }
        return 1;
    }

    private Integer awayTeamPointsFromBoardPoints(Integer doubledHomeBoardPoints, Integer doubledAwayBoardPoints) {
        if (doubledHomeBoardPoints == 0 && doubledAwayBoardPoints == 0) {
            return 0;
        }
        return 2 - homeTeamPointsFromBoardPoints(doubledHomeBoardPoints, doubledAwayBoardPoints);
    }

    private List<StandingsRowData> getStandingsRowData(List<TeamStandings> sortedTeamsStandings, Collection<MatchData> matches) {
        return sortedTeamsStandings.stream()
                .map(teamStandings -> new StandingsRowData(
                        new IdAndLabel(teamStandings.getTeam().id(), teamStandings.getTeam().name()),
                        getResultsForTeam(sortedTeamsStandings, teamStandings.getTeam().id(), matches),
                        teamStandings.getTeamPoints().toString(),
                        doubledBoardPointsToString(teamStandings.getDoubledBoardPoints())))
                .toList();
    }

    private String doubledBoardPointsToString(Integer doubledBoardPoints) {
        if (doubledBoardPoints % 2 == 0) {
            return String.valueOf(doubledBoardPoints / 2);
        }
        return (doubledBoardPoints - 1) / 2 + ",5";
    }

    private List<IdAndLabel> getResultsForTeam(List<TeamStandings> sortedTeamStandings,
                                               Long teamId,
                                               Collection<MatchData> matches) {
        List<IdAndLabel> resultsForTeam = new ArrayList<>();
        for (TeamStandings teamStandings : sortedTeamStandings) {
            if (teamStandings.getTeam().id().equals(teamId)) {
                resultsForTeam.add(new IdAndLabel(-1L, vsSelf));
            } else {
                for (MatchData matchData : matches) {
                    if (matchData.homeTeamId().equals(teamId)
                            && matchData.awayTeamId().equals(teamStandings.getTeam().id())) {
                        Integer doubledHomeBoardPoints = doubledHomeBoardPoints(matchData);
                        Integer doubledAwayBoardPoints = doubledAwayBoardPoints(matchData);
                        if (matchData.matchState() == MatchState.WIN_HOME_BY_FORFEIT) {
                            resultsForTeam.add(new IdAndLabel(matchData.id(), "+"));
                        } else if (Set.of(MatchState.WIN_AWAY_BY_FORFEIT, MatchState.BOTH_LOSE_BY_FORFEIT)
                                .contains(matchData.matchState())) {
                            resultsForTeam.add(new IdAndLabel(matchData.id(), "-"));
                        } else if (doubledHomeBoardPoints.equals(0) && doubledAwayBoardPoints.equals(0)) {
                            resultsForTeam.add(new IdAndLabel(matchData.id(), " "));
                        } else {
                            resultsForTeam.add(new IdAndLabel(
                                    matchData.id(), doubledBoardPointsToString(doubledHomeBoardPoints)));
                        }
                    }
                    if(matchData.awayTeamId().equals(teamId)
                            && matchData.homeTeamId().equals(teamStandings.getTeam().id())) {
                        Integer doubledHomeBoardPoints = doubledHomeBoardPoints(matchData);
                        Integer doubledAwayBoardPoints = doubledAwayBoardPoints(matchData);
                        if (matchData.matchState() == MatchState.WIN_AWAY_BY_FORFEIT) {
                            resultsForTeam.add(new IdAndLabel(matchData.id(), "+"));
                        } else if (Set.of(MatchState.WIN_HOME_BY_FORFEIT, MatchState.BOTH_LOSE_BY_FORFEIT)
                                .contains(matchData.matchState())) {
                            resultsForTeam.add(new IdAndLabel(matchData.id(), "-"));
                        } else if (doubledHomeBoardPoints.equals(0) && doubledAwayBoardPoints.equals(0)) {
                            resultsForTeam.add(new IdAndLabel(matchData.id(), " "));
                        } else {
                            resultsForTeam.add(new IdAndLabel(
                                    matchData.id(), doubledBoardPointsToString(doubledAwayBoardPoints)));
                        }
                    }
                }
            }
        }
        return resultsForTeam;
    }

    @NonNull
    public String assembleTextualStandings(@NonNull String seasonName, @NonNull String divisionName) {
        DivisionData divisionData = divisionService.getDivisionDataBySeasonNameAndDivisionName(seasonName, divisionName);
        StandingsData standingsData = assembleStandings(divisionData.id());
        int maxTeamNameLength = standingsData.rows().stream()
                .map(StandingsRowData::team)
                .map(IdAndLabel::label)
                .mapToInt(String::length)
                .max()
                .orElse(0);
        return buildTextualHeaderLine(standingsData, maxTeamNameLength) + "\n"
                + buildTextualStandingsRows(standingsData, maxTeamNameLength);
    }

    private String buildTextualHeaderLine(StandingsData standingsData, int maxTeamNameLength) {
        StringBuilder builder = new StringBuilder("Nr").append('\t')
                .append("Mannschaft ");

        builder.append(" ".repeat(Math.max(0, maxTeamNameLength - 10)));
        builder.append('\t');
        for(var i = 1; i<= standingsData.rows().size(); i++) {
            builder.append(i).append('\t');
        }
        builder.append("MP").append('\t');
        builder.append("BP").append('\t');
        return builder.toString();
    }

    private String buildTextualStandingsRows(StandingsData standingsData, int maxTeamNameLength) {
        StringBuilder builder = new StringBuilder();
        var i = 1;
        for (var row : standingsData.rows()) {
            builder.append(i++).append('\t').append(buildTextualStandingsRow(row, maxTeamNameLength)).append('\n');
        }
        return builder.toString();
    }

    private String buildTextualStandingsRow(StandingsRowData standingsRowData, int maxTeamNameLength) {
        StringBuilder builder = new StringBuilder(standingsRowData.team().label());
        builder.append(" ".repeat(Math.max(0, maxTeamNameLength - standingsRowData.team().label().length())));
        builder.append('\t');
        for (var result : standingsRowData.results()) {
            builder.append(result.label()).append('\t');
        }
        builder.append(standingsRowData.teamPoints()).append('\t');
        builder.append(standingsRowData.boardPoints()).append('\t');
        return builder.toString();
    }
}
