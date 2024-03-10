package de.tonypsilon.bmm.backend.participant.results.service;

import de.tonypsilon.bmm.backend.datatypes.IdAndLabel;
import de.tonypsilon.bmm.backend.game.data.GameData;
import de.tonypsilon.bmm.backend.game.service.GameService;
import de.tonypsilon.bmm.backend.game.service.Result;
import de.tonypsilon.bmm.backend.match.data.MatchData;
import de.tonypsilon.bmm.backend.match.service.MatchService;
import de.tonypsilon.bmm.backend.matchday.data.MatchdayData;
import de.tonypsilon.bmm.backend.matchday.service.MatchdayService;
import de.tonypsilon.bmm.backend.participant.data.ParticipantData;
import de.tonypsilon.bmm.backend.participant.results.data.ParticipantResultData;
import de.tonypsilon.bmm.backend.participant.results.data.ParticipantResultsData;
import de.tonypsilon.bmm.backend.participant.service.ParticipantService;
import de.tonypsilon.bmm.backend.participationeligibility.data.ParticipationEligibilityData;
import de.tonypsilon.bmm.backend.participationeligibility.service.ParticipationEligibilityService;
import de.tonypsilon.bmm.backend.team.data.TeamData;
import de.tonypsilon.bmm.backend.team.service.TeamService;
import org.springframework.stereotype.Service;

import java.util.Comparator;
import java.util.List;

@Service
public class ParticipantResultsService {

    private final GameService gameService;
    private final ParticipantService participantService;
    private final ParticipationEligibilityService participationEligibilityService;
    private final TeamService teamService;
    private final MatchService matchService;
    private final MatchdayService matchdayService;

    public ParticipantResultsService(GameService gameService,
                                     ParticipantService participantService,
                                     ParticipationEligibilityService participationEligibilityService,
                                     TeamService teamService,
                                     MatchService matchService,
                                     MatchdayService matchdayService) {
        this.gameService = gameService;
        this.participantService = participantService;
        this.participationEligibilityService = participationEligibilityService;
        this.teamService = teamService;
        this.matchService = matchService;
        this.matchdayService = matchdayService;
    }

    public ParticipantResultsData getResultsForParticipant(Long id) {
        ParticipantData participant = participantService.getParticipantById(id);
        ParticipationEligibilityData participationEligibility = participationEligibilityService
                .getParticipationEligibilityById(participant.participationEligibilityId());
        TeamData team = teamService.getTeamDataById(participant.teamId());
        List<GameData> gamesOfParticipant = gameService.findByParticipantId(id);

        return new ParticipantResultsData(
                participationEligibility.forename() + " " + participationEligibility.surname(),
                String.valueOf(participationEligibility.dwz()),
                new IdAndLabel(team.id(), team.name()),
                gamesOfParticipant.stream()
                        .sorted(Comparator.comparingInt(gameData ->
                                matchdayService.getMatchdayDataById(
                                        matchService.getMatchDataById(gameData.matchId()).matchdayId())
                                        .round()))
                        .map(gameData -> gameDataToParticipantResultData(gameData, id))
                        .toList(),
                "",
                "",
                participationEligibility.seasonId()
        );
    }

    private ParticipantResultData gameDataToParticipantResultData(GameData gameData, Long participantId) {
        MatchData matchData = matchService.getMatchDataById(gameData.matchId());
        MatchdayData matchdayData = matchdayService.getMatchdayDataById(matchData.matchdayId());
        boolean isHome = gameData.homeParticipantId().equals(participantId);
        ParticipantData opponent = participantService.getParticipantById(
                isHome ? gameData.awayParticipantId() : gameData.homeParticipantId());
        ParticipationEligibilityData opponentEligibility = participationEligibilityService
                .getParticipationEligibilityById(opponent.participationEligibilityId());
        TeamData opponentTeam = teamService.getTeamDataById(isHome ? matchData.awayTeamId() : matchData.homeTeamId());
        return new ParticipantResultData(
                matchdayData.round(),
                getColourByBoard(isHome, gameData.boardNumber()),
                new IdAndLabel(opponentTeam.id(), opponentTeam.name()),
                gameData.boardNumber(),
                new IdAndLabel(opponent.id(),
                        opponentEligibility.forename() + " " + opponentEligibility.surname()
                                + (opponentEligibility.dwz() != null
                                ? " (" + opponentEligibility.dwz() + ")"
                                : "")),
                getResultLabel(gameData, isHome)
        );
    }

    private String getColourByBoard(boolean isHome, int boardNumber) {
        if (isHome) {
            return boardNumber % 2 == 0 ? "white" : "black";
        } else {
            return boardNumber % 2 == 0 ? "black" : "white";
        }
    }

    private String getResultLabel(GameData gameData, boolean isHome) {
        if (isHome) {
            return gameData.overruledResultHome().map(Result::getLabel)
                    .or(() -> gameData.playedResultHome().map(Result::getLabel))
                    .orElse("");
        } else {
            return gameData.overruledResultAway().map(Result::getLabel)
                    .or(() -> gameData.playedResultAway().map(Result::getLabel))
                    .orElse("");
        }
    }

}
