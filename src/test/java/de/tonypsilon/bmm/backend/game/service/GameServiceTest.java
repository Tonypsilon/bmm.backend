package de.tonypsilon.bmm.backend.game.service;

import de.tonypsilon.bmm.backend.exception.AlreadyExistsException;
import de.tonypsilon.bmm.backend.exception.BadDataException;
import de.tonypsilon.bmm.backend.game.data.*;
import de.tonypsilon.bmm.backend.match.data.MatchData;
import de.tonypsilon.bmm.backend.match.data.MatchState;
import de.tonypsilon.bmm.backend.match.service.MatchService;
import de.tonypsilon.bmm.backend.matchday.service.MatchdayService;
import de.tonypsilon.bmm.backend.participant.data.ParticipantData;
import de.tonypsilon.bmm.backend.participant.service.ParticipantService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.*;
import static org.junit.jupiter.api.Assertions.assertThrows;

class GameServiceTest {

    private final GameRepository gameRepository = mock(GameRepository.class);
    private final MatchService matchService = mock(MatchService.class);
    private final MatchdayService matchdayService = mock(MatchdayService.class);
    private final ParticipantService participantService = mock(ParticipantService.class);
    private GameService gameService;
    private final Long matchId = 2L;
    private final Long homeParticipantId = 1L;
    private final Long awayParticipantId = 2L;
    private final Long homeTeamId = 3L;
    private final Long awayTeamId = 4L;
    private final GameCreationData creationData = new GameCreationData(matchId, 1,
            homeParticipantId, awayParticipantId,
            Optional.of(Result.WIN),Optional.empty(),
            Optional.of(Result.LOSS),Optional.empty());
    private final MatchData matchData = new MatchData(matchId, 5L, homeTeamId, awayTeamId, Optional.empty(),
            5, 3, Optional.empty(), Optional.empty(), Optional.empty(), null, MatchState.OPEN);
    private Game game;

    @BeforeEach
    void setUp() {
        gameService = new GameService(gameRepository,
                matchService,
                matchdayService,
                participantService);
        game = new Game();
        game.setId(9L);
        game.setMatchId(matchId);
        game.setBoardNumber(1);
        game.setHomeParticipantId(homeParticipantId);
        game.setAwayParticipantId(awayParticipantId);
        game.setPlayedResultHome(Result.WIN);
        game.setOverruledResultHome(null);
        game.setPlayedResultAway(Result.LOSS);
        game.setOverruledResultAway(null);
    }

    @Test
    void testCreateGameHomePlayerDoesntMatchTeam() {
        when(matchService.getMatchById(2L)).thenReturn(matchData);
        when(participantService.getParticipantById(homeParticipantId))
                .thenReturn(new ParticipantData(homeParticipantId, homeTeamId +1, homeParticipantId, 3));
        BadDataException actualException = assertThrows(BadDataException.class,
                () -> gameService.createGame(creationData));
        assertThat(actualException).hasMessage("Der Spieler mit ID 1 ist nicht vom Team mit ID 3!");
    }

    @Test
    void testCreateGameAwayPlayerDoesntMatchTeam() {
        when(matchService.getMatchById(2L)).thenReturn(matchData);
        when(participantService.getParticipantById(homeParticipantId))
                .thenReturn(new ParticipantData(homeParticipantId, homeTeamId, homeParticipantId, 3));
        when(participantService.getParticipantById(awayParticipantId))
                .thenReturn(new ParticipantData(awayParticipantId, awayTeamId +1, awayParticipantId, 3));
        BadDataException actualException = assertThrows(BadDataException.class,
                () -> gameService.createGame(creationData));
        assertThat(actualException).hasMessage("Der Spieler mit ID 2 ist nicht vom Team mit ID 4!");
    }

    @ParameterizedTest
    @ValueSource(ints = {-1, 0, 9})
    void testCreateGameInvalidBoardNumber(int boardNumber) {
        GameCreationData creationData = new GameCreationData(matchId, boardNumber,
                homeParticipantId, awayParticipantId,
                Optional.of(Result.WIN),Optional.empty(),
                Optional.of(Result.LOSS),Optional.empty());
        when(matchService.getMatchById(2L)).thenReturn(matchData);
        when(participantService.getParticipantById(homeParticipantId))
                .thenReturn(new ParticipantData(homeParticipantId, homeTeamId, homeParticipantId, 3));
        when(participantService.getParticipantById(awayParticipantId))
                .thenReturn(new ParticipantData(awayParticipantId, awayTeamId, awayParticipantId, 3));
        when(matchdayService.getNumberOfBoardsForMatchday(5L)).thenReturn(8);
        BadDataException actualException = assertThrows(BadDataException.class,
                () -> gameService.createGame(creationData));
        assertThat(actualException).hasMessage("Die Brettnummer ist ungültig!");
    }

    @Test
    void testCreateGameNullBoardNumber() {
        GameCreationData creationData = new GameCreationData(matchId, null,
                homeParticipantId, awayParticipantId,
                Optional.of(Result.WIN),Optional.empty(),
                Optional.of(Result.LOSS),Optional.empty());
        when(matchService.getMatchById(2L)).thenReturn(matchData);
        when(participantService.getParticipantById(homeParticipantId))
                .thenReturn(new ParticipantData(homeParticipantId, homeTeamId, homeParticipantId, 3));
        when(participantService.getParticipantById(awayParticipantId))
                .thenReturn(new ParticipantData(awayParticipantId, awayTeamId, awayParticipantId, 3));
        when(matchdayService.getNumberOfBoardsForMatchday(5L)).thenReturn(8);
        BadDataException actualException = assertThrows(BadDataException.class,
                () -> gameService.createGame(creationData));
        assertThat(actualException).hasMessage("Die Brettnummer ist ungültig!");
    }

    @Test
    void testCreateGameAlreadyExists() {
        when(matchService.getMatchById(2L)).thenReturn(matchData);
        when(participantService.getParticipantById(homeParticipantId))
                .thenReturn(new ParticipantData(homeParticipantId, homeTeamId, homeParticipantId, 3));
        when(participantService.getParticipantById(awayParticipantId))
                .thenReturn(new ParticipantData(awayParticipantId, awayTeamId, awayParticipantId, 3));
        when(matchdayService.getNumberOfBoardsForMatchday(5L)).thenReturn(8);
        when(gameRepository.existsByMatchIdAndBoardNumber(matchId, 1)).thenReturn(Boolean.TRUE);
        AlreadyExistsException actualException = assertThrows(AlreadyExistsException.class,
                () -> gameService.createGame(creationData));
        assertThat(actualException)
                .hasMessage("Es gibt für den Mannschaftskampf mit der ID 2 bereits ein Spiel für Brett Nummer 1!");
    }

    @Test
    void testCreateGameOk() {
        when(matchService.getMatchById(2L)).thenReturn(matchData);
        when(participantService.getParticipantById(homeParticipantId))
                .thenReturn(new ParticipantData(homeParticipantId, homeTeamId, homeParticipantId, 3));
        when(participantService.getParticipantById(awayParticipantId))
                .thenReturn(new ParticipantData(awayParticipantId, awayTeamId, awayParticipantId, 3));
        when(matchdayService.getNumberOfBoardsForMatchday(5L)).thenReturn(8);
        when(gameRepository.existsByMatchIdAndBoardNumber(matchId, 1)).thenReturn(Boolean.FALSE);
        when(gameRepository.getByMatchIdAndBoardNumber(matchId, 1)).thenReturn(game);

        GameData actual = gameService.createGame(creationData);
        assertThat(actual.id()).isEqualTo(9L);
        assertThat(actual.matchId()).isEqualTo(matchId);
        assertThat(actual.boardNumber()).isEqualTo(1);
        assertThat(actual.homeParticipantId()).isEqualTo(homeParticipantId);
        assertThat(actual.awayParticipantId()).isEqualTo(awayParticipantId);
        assertThat(actual.playedResultHome()).isEqualTo(Optional.of(Result.WIN));
        assertThat(actual.overruledResultHome()).isEmpty();
        assertThat(actual.playedResultAway()).isEqualTo(Optional.of(Result.LOSS));
        assertThat(actual.overruledResultAway()).isEmpty();
    }

}