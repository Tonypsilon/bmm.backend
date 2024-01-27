package de.tonypsilon.bmm.backend.match.service;

import de.tonypsilon.bmm.backend.datatypes.IdAndLabel;
import de.tonypsilon.bmm.backend.game.data.GameData;
import de.tonypsilon.bmm.backend.game.service.GameService;
import de.tonypsilon.bmm.backend.game.service.Result;
import de.tonypsilon.bmm.backend.match.data.*;
import de.tonypsilon.bmm.backend.matchday.service.MatchdayService;
import de.tonypsilon.bmm.backend.participant.data.ParticipantData;
import de.tonypsilon.bmm.backend.participant.service.ParticipantService;
import de.tonypsilon.bmm.backend.participationeligibility.data.ParticipationEligibilityData;
import de.tonypsilon.bmm.backend.participationeligibility.service.ParticipationEligibilityService;
import de.tonypsilon.bmm.backend.referee.service.RefereeService;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Service
public class RichMatchInformationAssemblyService {

    private final MatchService matchService;
    private final ParticipantService participantService;
    private final ParticipationEligibilityService participationEligibilityService;
    private final GameService gameService;
    private final MatchdayService matchdayService;
    private final RefereeService refereeService;

    public RichMatchInformationAssemblyService(final MatchService matchService,
                                               final ParticipantService participantService,
                                               final ParticipationEligibilityService participationEligibilityService,
                                               final GameService gameService,
                                               final MatchdayService matchdayService,
                                               final RefereeService refereeService) {
        this.matchService = matchService;
        this.participantService = participantService;
        this.participationEligibilityService = participationEligibilityService;
        this.gameService = gameService;
        this.matchdayService = matchdayService;
        this.refereeService = refereeService;
    }

    @Transactional
    @NonNull
    public RichMatchData assembleRichMatchData(@NonNull Long matchId) {
        MatchData matchData = matchService.getMatchDataById(matchId);
        List<ParticipantDataForClient> availableHomePlayers =
                participantService.getParticipantsEligibleForTeam(matchData.homeTeamId()).stream()
                        .map(this::participantDataToParticipantDataForClient)
                        .toList();
        List<ParticipantDataForClient> availableAwayPlayers =
                participantService.getParticipantsEligibleForTeam(matchData.awayTeamId()).stream()
                        .map(this::participantDataToParticipantDataForClient)
                        .toList();
        List<GameData> existingGames = gameService.getByMatchId(matchId);
        List<ParticipantDataForClient> selectedHomePlayers = existingGames.stream()
                .map(GameData::homeParticipantId)
                .map(participantService::getParticipantById)
                .map(this::participantDataToParticipantDataForClient)
                .toList();
        List<ParticipantDataForClient> selectedAwayPlayers = existingGames.stream()
                .map(GameData::awayParticipantId)
                .map(participantService::getParticipantById)
                .map(this::participantDataToParticipantDataForClient)
                .toList();
        Integer numberOfBoards = matchdayService.getNumberOfBoardsForMatchday(matchData.matchdayId());
        List<IdAndLabel> availableReferees = refereeService.findBySeasonId(
                matchdayService.getSeasonIdForMatchday(matchData.matchdayId())).stream()
                .map(refereeData -> new IdAndLabel(
                        refereeData.id(),
                        refereeData.surname() + ", " + refereeData.forename()))
                .toList();
        IdAndLabel selectedReferee = matchData.refereeId()
                .map(refereeService::getById)
                .map(refereeData -> new IdAndLabel(
                        refereeData.id(),
                        refereeData.surname() + ", " + refereeData.forename()))
                .orElse(null);
        List<GameDataForClient> results = existingGames.stream()
                .map(this::gameDataToResultDataForClient)
                .toList();
        return new RichMatchData(
                availableHomePlayers,
                availableAwayPlayers,
                selectedHomePlayers,
                selectedAwayPlayers,
                numberOfBoards,
                availableReferees,
                selectedReferee,
                results
        );
    }

    private ParticipantDataForClient participantDataToParticipantDataForClient(ParticipantData participantData) {
        ParticipationEligibilityData participationEligibilityData =
                participationEligibilityService.getParticipationEligibilityById(
                        participantData.participationEligibilityId());
        return new ParticipantDataForClient(
                participantData.id(),
                participantService.getCodeOfParticipant(participantData.id()),
                participationEligibilityData.forename(),
                participationEligibilityData.surname(),
                participationEligibilityData.dwz());
    }

    @NonNull
    public GameDataForClient gameDataToResultDataForClient(@NonNull GameData gameData) {
        return new GameDataForClient(
                participantDataToParticipantDataForClient(
                        participantService.getParticipantById(gameData.homeParticipantId())),
                participantDataToParticipantDataForClient(
                        participantService.getParticipantById(gameData.awayParticipantId())),
                new ResultDataForClient(resultFromGameData(gameData),
                gameData.overruledResultHome().map(Result::getDoubledValue)
                        .or(() -> gameData.playedResultHome().map(Result::getDoubledValue))
                        .orElse(null),
                gameData.overruledResultAway().map(Result::getDoubledValue)
                        .or(() -> gameData.playedResultAway().map(Result::getDoubledValue))
                        .orElse(null)));
    }

    private String resultFromGameData(GameData gameData) {
        String home = gameData.overruledResultHome().map(Result::getLabel)
                .or(() -> gameData.playedResultHome().map(Result::getLabel))
                .orElse("?");
        String away = gameData.overruledResultAway().map(Result::getLabel)
                .or(() -> gameData.playedResultAway().map(Result::getLabel))
                .orElse("?");
        return home + " : " + away;
    }

}
