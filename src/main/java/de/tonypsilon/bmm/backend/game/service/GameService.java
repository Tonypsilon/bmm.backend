package de.tonypsilon.bmm.backend.game.service;

import de.tonypsilon.bmm.backend.exception.AlreadyExistsException;
import de.tonypsilon.bmm.backend.exception.BadDataException;
import de.tonypsilon.bmm.backend.game.data.*;
import de.tonypsilon.bmm.backend.match.data.MatchData;
import de.tonypsilon.bmm.backend.match.service.MatchService;
import de.tonypsilon.bmm.backend.matchday.service.MatchdayService;
import de.tonypsilon.bmm.backend.participant.data.ParticipantData;
import de.tonypsilon.bmm.backend.participant.service.ParticipantService;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
public class GameService {

    private final GameRepository gameRepository;
    private final MatchService matchService;
    private final MatchdayService matchdayService;
    private final ParticipantService participantService;

    public GameService(final GameRepository gameRepository,
                       final MatchService matchService,
                       final MatchdayService matchdayService,
                       final ParticipantService participantService) {
        this.gameRepository = gameRepository;
        this.matchService = matchService;
        this.matchdayService = matchdayService;
        this.participantService = participantService;
    }

    @Transactional
    @NonNull
    public GameData createGame(GameCreationData gameCreationData) {
        MatchData matchData = matchService.getMatchDataById(gameCreationData.matchId());
        ParticipantData homeParticipantData = participantService.getParticipantById(gameCreationData.homeParticipantId());
        ParticipantData awayParticipantData = participantService.getParticipantById(gameCreationData.awayParticipantId());

        verifyPlayerMatchesTeam(homeParticipantData, matchData.homeTeamId());
        verifyPlayerMatchesTeam(awayParticipantData, matchData.awayTeamId());

        Integer numberOfBoardsOfDivision = matchdayService.getNumberOfBoardsForMatchday(matchData.matchdayId());
        if(gameCreationData.boardNumber() == null
                || gameCreationData.boardNumber() < 1
                || gameCreationData.boardNumber() >= numberOfBoardsOfDivision) {
            throw new BadDataException("Die Brettnummer ist ungültig!");
        }

        if(gameRepository.existsByMatchIdAndBoardNumber(gameCreationData.matchId(), gameCreationData.boardNumber())) {
            throw new AlreadyExistsException(
                    "Es gibt für den Mannschaftskampf mit der ID %d bereits ein Spiel für Brett Nummer %d!"
                            .formatted(gameCreationData.matchId(), gameCreationData.boardNumber()));
        }

        return gameToGameData(gameRepository.getByMatchIdAndBoardNumber(gameCreationData.matchId(), gameCreationData.boardNumber()));
    }

    private void verifyPlayerMatchesTeam(ParticipantData participantData, Long teamId) {
        if(!participantData.teamId().equals(teamId)) {
            throw new BadDataException("Der Spieler mit ID %d ist nicht vom Team mit ID %d!"
                    .formatted(participantData.id(), teamId));
        }
    }

    @NonNull
    private GameData gameToGameData(Game game) {
        return new GameData(game.getId(),
                game.getMatchId(),
                game.getBoardNumber(),
                game.getHomeParticipantId(),
                game.getAwayParticipantId(),
                game.getPlayedResultHome(),
                game.getOverruledResultHome(),
                game.getPlayedResultAway(),
                game.getOverruledResultAway());
    }
}
