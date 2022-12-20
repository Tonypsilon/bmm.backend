package de.tonypsilon.bmm.backend.game.service;

import de.tonypsilon.bmm.backend.exception.AlreadyExistsException;
import de.tonypsilon.bmm.backend.exception.BadDataException;
import de.tonypsilon.bmm.backend.game.data.*;
import de.tonypsilon.bmm.backend.match.data.MatchData;
import de.tonypsilon.bmm.backend.match.service.MatchService;
import de.tonypsilon.bmm.backend.participant.data.ParticipantData;
import de.tonypsilon.bmm.backend.participant.service.ParticipantService;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
public class GameService {

    private final GameRepository gameRepository;
    private final MatchService matchService;
    private final ParticipantService participantService;

    public GameService(final GameRepository gameRepository,
                       final MatchService matchService,
                       ParticipantService participantService) {
        this.gameRepository = gameRepository;
        this.matchService = matchService;
        this.participantService = participantService;
    }

    @Transactional
    @NonNull
    public GameData createGame(CreateGameData createGameData) {
        MatchData matchData = matchService.getMatchById(createGameData.matchId());
        ParticipantData homeParticipantData = participantService.getParticipantById(createGameData.homeParticipantId());
        ParticipantData awayParticipantData = participantService.getParticipantById(createGameData.awayParticipantId());

        verifyPlayerMatchesTeam(homeParticipantData, matchData.homeTeamId());
        verifyPlayerMatchesTeam(awayParticipantData, matchData.awayTeamId());

        if(gameRepository.existsByMatchIdAndBoardNumber(createGameData.matchId(), createGameData.boardNumber())) {
            throw new AlreadyExistsException(
                    "Es gibt für den Mannschaftskampf mit der ID %d bereits ein Spiel für Brett Nummer %d!"
                            .formatted(createGameData.matchId(), createGameData.boardNumber()));
        }

        // todo: verify board number

        return gameToGameData(gameRepository.getByMatchIdAndBoardNumber(createGameData.matchId(), createGameData.boardNumber()));
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
