package de.tonypsilon.bmm.backend.game.service;

import de.tonypsilon.bmm.backend.exception.AlreadyExistsException;
import de.tonypsilon.bmm.backend.exception.BadDataException;
import de.tonypsilon.bmm.backend.exception.NotFoundException;
import de.tonypsilon.bmm.backend.exception.SeasonStageException;
import de.tonypsilon.bmm.backend.game.data.Game;
import de.tonypsilon.bmm.backend.game.data.GameCreationData;
import de.tonypsilon.bmm.backend.game.data.GameData;
import de.tonypsilon.bmm.backend.game.data.GameRepository;
import de.tonypsilon.bmm.backend.match.data.MatchData;
import de.tonypsilon.bmm.backend.match.service.MatchService;
import de.tonypsilon.bmm.backend.matchday.service.MatchdayService;
import de.tonypsilon.bmm.backend.participant.data.ParticipantData;
import de.tonypsilon.bmm.backend.participant.service.ParticipantService;
import de.tonypsilon.bmm.backend.season.service.SeasonStage;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Objects;
import java.util.Optional;

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

        if(matchdayService.getSeasonStageOfMatchday(matchData.matchdayId()) != SeasonStage.RUNNING) {
            throw new SeasonStageException(
                    "Saison ist nicht in der Durchführungsphase, es kann kein Spiel angelegt werden!");
        }

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
        Game game = new Game();
        game.setMatchId(gameCreationData.matchId());
        game.setBoardNumber(gameCreationData.boardNumber());
        game.setHomeParticipantId(gameCreationData.homeParticipantId());
        game.setAwayParticipantId(gameCreationData.awayParticipantId());
        Optional.ofNullable(gameCreationData.playedResultHome()).ifPresent(game::setPlayedResultHome);
        Optional.ofNullable(gameCreationData.overruledResultHome()).ifPresent(game::setOverruledResultHome);
        Optional.ofNullable(gameCreationData.playedResultAway()).ifPresent(game::setPlayedResultAway);
        Optional.ofNullable(gameCreationData.overruledResultAway()).ifPresent(game::setOverruledResultAway);
        return gameToGameData(gameRepository.getByMatchIdAndBoardNumber(gameCreationData.matchId(), gameCreationData.boardNumber()));
    }

    @Transactional
    @NonNull
    public GameData changeResult(@NonNull Long gameId, @NonNull ResultData resultData) {
        Game game = getById(gameId);
        game.setPlayedResultHome(Objects.requireNonNull(resultData.homeResult()));
        game.setPlayedResultAway(Objects.requireNonNull(resultData.awayResult()));
        gameRepository.save(game);
        return gameToGameData(getById(gameId));
    }

    @Transactional
    @NonNull
    public GameData changeOverruledResult(@NonNull Long gameId, @NonNull ResultData overruledResultData) {
        Game game = getById(gameId);
        game.setOverruledResultHome(Objects.requireNonNull(overruledResultData.homeResult()));
        game.setOverruledResultAway(Objects.requireNonNull(overruledResultData.awayResult()));
        gameRepository.save(game);
        return gameToGameData(getById(gameId));
    }

    @Transactional
    public void deleteGame(@NonNull Long gameId) {
        gameRepository.deleteById(gameId);
    }

    @Transactional
    public void deleteAllGamesFromMatch(@NonNull Long matchId) {
        gameRepository.deleteByMatchId(matchId);
    }

    @Transactional
    public GameData getGameDataById(Long gameId) {
        return gameToGameData(getById(gameId));
    }

    private Game getById(Long gameId) {
        return gameRepository.findById(gameId)
                .orElseThrow(() -> new NotFoundException("Es gibt keine Begegnung mit der ID %d!"
                        .formatted(gameId)));
    }

    @Transactional
    @NonNull
    public List<GameData> getByMatchId(@NonNull Long matchId) {
        return gameRepository.findByMatchIdOrderByBoardNumberAsc(matchId).stream()
                .map(this::gameToGameData)
                .toList();
    }

    private void verifyPlayerMatchesTeam(ParticipantData participantData, Long teamId) {
        if(!participantService.getParticipantsEligibleForTeam(teamId).stream().map(ParticipantData::id)
                .toList().contains(participantData.id())) {
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
