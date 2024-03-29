package de.tonypsilon.bmm.backend.division.service;

import de.tonypsilon.bmm.backend.division.data.DivisionResultsData;
import de.tonypsilon.bmm.backend.exception.SeasonStageException;
import de.tonypsilon.bmm.backend.game.service.GameService;
import de.tonypsilon.bmm.backend.match.data.*;
import de.tonypsilon.bmm.backend.match.service.MatchResultService;
import de.tonypsilon.bmm.backend.match.service.MatchService;
import de.tonypsilon.bmm.backend.match.service.RichMatchInformationAssemblyService;
import de.tonypsilon.bmm.backend.matchday.data.MatchdayClientData;
import de.tonypsilon.bmm.backend.matchday.data.MatchdayData;
import de.tonypsilon.bmm.backend.matchday.service.MatchdayService;
import de.tonypsilon.bmm.backend.referee.service.RefereeService;
import de.tonypsilon.bmm.backend.season.service.SeasonService;
import de.tonypsilon.bmm.backend.season.service.SeasonStage;
import de.tonypsilon.bmm.backend.team.data.TeamData;
import de.tonypsilon.bmm.backend.team.service.TeamService;
import de.tonypsilon.bmm.backend.venue.data.VenueData;
import de.tonypsilon.bmm.backend.venue.service.VenueService;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Optional;
import java.util.Set;

@Service
public class DivisionResultsAssemblyService {

    private final SeasonService seasonService;
    private final DivisionService divisionService;
    private final MatchdayService matchdayService;
    private final VenueService venueService;
    private final TeamService teamService;
    private final MatchService matchService;
    private final RefereeService refereeService;
    private final RichMatchInformationAssemblyService richMatchInformationAssemblyService;
    private final MatchResultService matchResultService;
    private final GameService gameService;

    public DivisionResultsAssemblyService(
            final SeasonService seasonService,
            final DivisionService divisionService,
            final MatchdayService matchdayService,
            final VenueService venueService,
            final TeamService teamService,
            final MatchService matchService,
            final RefereeService refereeService,
            final RichMatchInformationAssemblyService richMatchInformationAssemblyService,
            final MatchResultService matchResultService,
            final GameService gameService) {
        this.seasonService = seasonService;
        this.divisionService = divisionService;
        this.matchdayService = matchdayService;
        this.venueService = venueService;
        this.teamService = teamService;
        this.matchService = matchService;
        this.refereeService = refereeService;
        this.richMatchInformationAssemblyService = richMatchInformationAssemblyService;
        this.matchResultService = matchResultService;
        this.gameService = gameService;
    }

    @NonNull
    @Transactional
    public DivisionResultsData assembleDivisionResults(@NonNull Long divisionId) {
        if(!Set.of(SeasonStage.REGISTRATION, SeasonStage.PREPARATION, SeasonStage.RUNNING, SeasonStage.COMPLETED)
                .contains(seasonService.getStageOfSeason(divisionService.getSeasonIdByDivisionId(divisionId)))) {
            throw new SeasonStageException("Die Saison ist archiviert!");
        }
        return new DivisionResultsData(assembleMatchdaysInformation(divisionId));
    }

    @NonNull
    private List<MatchdayClientData> assembleMatchdaysInformation(@NonNull Long divisionId) {
        List<MatchdayData> matchdays = matchdayService.getMatchdaysOfDivisionOrderedByRound(divisionId);
        return matchdays.stream()
                .map(this::matchdayDataToMatchdayClientData)
                .toList();
    }

    @NonNull
    private MatchdayClientData matchdayDataToMatchdayClientData(
            @NonNull MatchdayData matchdayData) {
        return new MatchdayClientData(
                matchdayData.date(),
                matchdayData.round(),
                getMatchesForMatchday(matchdayData.id()));
    }

    @NonNull
    private List<MatchResultClientData> getMatchesForMatchday(@NonNull Long matchdayId) {
        return matchService.findByMatchdayId(matchdayId).stream()
                .map(this::matchDataToMatchResultClientData)
                .toList();
    }

    @NonNull
    private MatchResultClientData matchDataToMatchResultClientData(@NonNull MatchData matchData) {
        TeamData homeTeam = teamService.getTeamDataById(matchData.homeTeamId());
        TeamData awayTeam = teamService.getTeamDataById(matchData.awayTeamId());
        MatchResultData summedResultOfMatch = matchResultService.getSummedResultOfMatch(matchData.id());
        return new MatchResultClientData(
                matchData.date().orElse(null),
                matchData.venueId().map(venueService::getVenueDataById).map(VenueData::address)
                        .orElseGet( () -> venueService.getVenueDataById(homeTeam.venueId()).address()),
                matchData.refereeId().stream()
                        .map(refereeService::findById)
                        .flatMap(Optional::stream)
                        .findAny()
                        .map(refereeData -> refereeData.surname() + ", " + refereeData.forename())
                        .orElse(null),
                homeTeam.name(),
                awayTeam.name(),
                matchData.matchState().name(),
                getGamesForMatch(matchData.id()),
                getTeamResultLabelHome(matchData.matchState(), summedResultOfMatch.homeTeamHalfBoardPoints()),
                getTeamResultLabelAway(matchData.matchState(), summedResultOfMatch.awayTeamHalfBoardPoints())
        );
    }

    @NonNull
    private String getTeamResultLabelHome(MatchState matchState, Integer homeTeamHalfBoardPoints) {
        return switch (matchState) {
            case WIN_HOME_BY_FORFEIT -> "+";
            case WIN_AWAY_BY_FORFEIT, BOTH_LOSE_BY_FORFEIT -> "-";
            default -> matchResultService.getLabelForBoardHalfPoints(homeTeamHalfBoardPoints);
        };
    }

    @NonNull
    private String getTeamResultLabelAway(MatchState matchState, Integer awayTeamHalfBoardPoints) {
        return switch (matchState) {
            case WIN_AWAY_BY_FORFEIT -> "+";
            case WIN_HOME_BY_FORFEIT, BOTH_LOSE_BY_FORFEIT -> "-";
            default -> matchResultService.getLabelForBoardHalfPoints(awayTeamHalfBoardPoints);
        };
    }

    @NonNull
    private List<GameDataForClient> getGamesForMatch(Long matchId) {
        return gameService.getByMatchId(matchId).stream()
                .map(richMatchInformationAssemblyService::gameDataToResultDataForClient)
                .toList();
    }

}
