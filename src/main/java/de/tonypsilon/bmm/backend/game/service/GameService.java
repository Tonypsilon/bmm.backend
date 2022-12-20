package de.tonypsilon.bmm.backend.game.service;

import de.tonypsilon.bmm.backend.exception.NotFoundException;
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
        MatchData matchData = matchService.findById(createGameData.matchId())
                .orElseThrow(() -> new NotFoundException("Es gibt keinen Mannschaftskampf mit der ID %d!"
                        .formatted(createGameData.matchId())));
        // If any of the participants does not exist, ParticipantService throws a NotFoundException
        ParticipantData homeParticipantData = participantService.getParticipantById(createGameData.homeParticipantId());
        ParticipantData awayParticipantData = participantService.getParticipantById(createGameData.awayParticipantId());

        return gameToGameData(gameRepository.getByMatchIdAndBoardNumber(createGameData.matchId(), createGameData.boardNumber()));
    }

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
