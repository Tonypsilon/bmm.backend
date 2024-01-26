package de.tonypsilon.bmm.backend.standings.service;

import de.tonypsilon.bmm.backend.datatypes.IdAndLabel;
import de.tonypsilon.bmm.backend.division.data.DivisionData;
import de.tonypsilon.bmm.backend.division.service.DivisionService;
import de.tonypsilon.bmm.backend.game.service.GameService;
import de.tonypsilon.bmm.backend.match.data.ParticipantDataForClient;
import de.tonypsilon.bmm.backend.match.service.MatchService;
import de.tonypsilon.bmm.backend.matchday.data.MatchdayData;
import de.tonypsilon.bmm.backend.matchday.service.MatchdayService;
import de.tonypsilon.bmm.backend.participant.data.ParticipantData;
import de.tonypsilon.bmm.backend.participant.service.ParticipantService;
import de.tonypsilon.bmm.backend.participationeligibility.data.ParticipationEligibilityData;
import de.tonypsilon.bmm.backend.participationeligibility.service.ParticipationEligibilityService;
import de.tonypsilon.bmm.backend.standings.data.*;
import de.tonypsilon.bmm.backend.team.data.TeamData;
import de.tonypsilon.bmm.backend.team.data.TeamDivisionLinkData;
import de.tonypsilon.bmm.backend.team.service.TeamDivisionLinkService;
import de.tonypsilon.bmm.backend.team.service.TeamService;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Service;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

@Service
public class ProgressChartAssembler {

    private final DivisionService divisionService;
    private final TeamDivisionLinkService teamDivisionLinkService;
    private final TeamService teamService;
    private final ParticipantService participantService;
    private final ParticipationEligibilityService participationEligibilityService;
    private final MatchdayService matchdayService;
    private final MatchService matchService;
    private final GameService gameService;

    public ProgressChartAssembler(final DivisionService divisionService,
                                  final TeamDivisionLinkService teamDivisionLinkService,
                                  final TeamService teamService,
                                  final ParticipantService participantService,
                                  final ParticipationEligibilityService participationEligibilityService,
                                  final MatchdayService matchdayService,
                                  final MatchService matchService,
                                  final GameService gameService) {
        this.divisionService = divisionService;
        this.teamDivisionLinkService = teamDivisionLinkService;
        this.teamService = teamService;
        this.participantService = participantService;
        this.participationEligibilityService = participationEligibilityService;
        this.matchdayService = matchdayService;
        this.matchService = matchService;
        this.gameService = gameService;
    }

    @NonNull
    public ProgressChartData assembleProgressChart(@NonNull Long divisionId) {
        DivisionData divisionData = divisionService.getDivisionDataById(divisionId);
        List<TeamData> teams = teamDivisionLinkService.getByDivisionId(divisionId).stream()
                .sorted(Comparator.comparingInt(TeamDivisionLinkData::number))
                .map(TeamDivisionLinkData::teamId)
                .map(teamService::getTeamDataById)
                .toList();
        Map<Integer, Collection<ProgressChartGameContext>> gamesByRound = matchdayService
                .getMatchdaysOfDivisionOrderedByRound(divisionId).stream()
                .collect(Collectors.toMap(
                        MatchdayData::round,
                        matchdayData -> matchService.findByMatchdayId(matchdayData.id()).stream()
                                .flatMap(matchData -> gameService.getByMatchId(matchdayData.id())
                                        .stream()
                                        .map(gameData ->
                                                new ProgressChartGameContext(gameData,
                                                        matchData.homeTeamId(), matchData.awayTeamId())))
                                .toList()));
        ProgressChartAssemblingContext context =
                new ProgressChartAssemblingContext(divisionData.numberOfTeams()-1, gamesByRound);
        return new ProgressChartData(
                context.getNumberOfRounds(),
                teams.stream()
                        .map(teamData -> assembleProgressChartDataForTeam(teamData, context))
                        .toList()
        );
    }

    private TeamProgressChartData assembleProgressChartDataForTeam(@NonNull TeamData teamData,
                                                                   @NonNull ProgressChartAssemblingContext context) {
        List<ParticipantData> participantsOfTeam =
                participantService.getParticipantsEligibleForTeam(teamData.id());
        return new TeamProgressChartData(
                new IdAndLabel(teamData.id(), teamData.name()),
                participantsOfTeam.stream()
                        .map(participantData -> this.assembleParticipantProgressChartData(participantData, context))
                        .toList()
        );
    }

    @NonNull
    private ParticipantProgressChartData assembleParticipantProgressChartData(
            @NonNull ParticipantData participantData,
            @NonNull ProgressChartAssemblingContext context) {
        ParticipationEligibilityData participationEligibilityData =
                participationEligibilityService.getParticipationEligibilityById(
                        participantData.participationEligibilityId());
        return new ParticipantProgressChartData(
                new ParticipantDataForClient(participantData.id(),
                        participantService.getCodeOfParticipant(participantData.id()),
                        participationEligibilityData.forename(),
                        participationEligibilityData.surname(),
                        participationEligibilityData.dwz()),
                IntStream.range(1, context.getNumberOfRounds()+1)
                        .mapToObj(round -> context.getGame(participantData.id(), participantData.teamId(), round))
                        .toList()
        );
    }
}
