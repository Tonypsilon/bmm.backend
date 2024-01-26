package de.tonypsilon.bmm.backend.season.service;

import de.tonypsilon.bmm.backend.exception.AlreadyExistsException;
import de.tonypsilon.bmm.backend.exception.BadDataException;
import de.tonypsilon.bmm.backend.exception.SeasonStageException;
import de.tonypsilon.bmm.backend.season.data.*;
import de.tonypsilon.bmm.backend.validation.service.ValidationService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;
import org.junit.jupiter.params.provider.NullAndEmptySource;
import org.junit.jupiter.params.provider.ValueSource;

import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class PlayingDateServiceTest {

    private final PlayingDateRepository playingDateRepository = mock(PlayingDateRepository.class);
    private final SeasonService seasonService = mock(SeasonService.class);
    private final ValidationService validationService = new ValidationService();
    private PlayingDateService playingDateService;
    private PlayingDate playingDate;

    @BeforeEach
    void setUp() {
        playingDateService = new PlayingDateService(playingDateRepository,
                seasonService, validationService);
        playingDate = new PlayingDate();
        playingDate.setId(1L);
        playingDate.setSeasonId(1L);
        playingDate.setNumber(2);
        playingDate.setDate("1.1.2001");
    }

    @ParameterizedTest
    @EnumSource(value = SeasonStage.class,
            mode = EnumSource.Mode.EXCLUDE,
            names = {"REGISTRATION", "PREPARATION"})
    void testCreatePlayingDateWrongSeasonStage(SeasonStage stage) {
        PlayingDateCreationData creationData = new PlayingDateCreationData(1L, 1, "1.1.2001");
        when(seasonService.getStageOfSeason(1L)).thenReturn(stage);
        SeasonStageException actualException = assertThrows(SeasonStageException.class,
                () -> playingDateService.createPlayingDate(creationData));
        assertThat(actualException)
                .hasMessage("Die Saison ist nicht in der Registrierungs- oder Vorbereitungsphase!");
    }

    @ParameterizedTest
    @ValueSource(ints = {-1, 0})
    void testCreatePlayingDateNonPositiveNumber(int number) {
        PlayingDateCreationData creationData = new PlayingDateCreationData(1L, number, "1.1.2001");
        when(seasonService.getStageOfSeason(1L)).thenReturn(SeasonStage.PREPARATION);
        BadDataException actualException = assertThrows(BadDataException.class,
                () -> playingDateService.createPlayingDate(creationData));
        assertThat(actualException).hasMessage("Die Runde muss eine positive Zahl sein!");
    }

    @Test
    void testCreatePlayingDateAlreadyExists() {
        PlayingDateCreationData creationData = new PlayingDateCreationData(1L, 2, "1.1.2001");
        when(seasonService.getStageOfSeason(1L)).thenReturn(SeasonStage.PREPARATION);
        when(playingDateRepository.existsBySeasonIdAndNumber(1L, 2)).thenReturn(Boolean.TRUE);
        AlreadyExistsException actualException = assertThrows(AlreadyExistsException.class,
                () -> playingDateService.createPlayingDate(creationData));
        assertThat(actualException).hasMessage("Es gibt bereits ein Datum für Saison 1 und Runde 2!");
    }

    @ParameterizedTest
    @ValueSource(strings = {"1.1.2001#", "@"})
    void testCreatePlayingDateInvalidDateCharacters(String date) {
        PlayingDateCreationData creationData = new PlayingDateCreationData(1L, 2, date);
        when(seasonService.getStageOfSeason(1L)).thenReturn(SeasonStage.PREPARATION);
        when(playingDateRepository.existsBySeasonIdAndNumber(1L, 2)).thenReturn(Boolean.FALSE);
        BadDataException actualException = assertThrows(BadDataException.class,
                () -> playingDateService.createPlayingDate(creationData));
        assertThat(actualException).hasMessage("Das Datum enthält ungültige Zeichen!");
    }

    @ParameterizedTest
    @NullAndEmptySource
    void testCreatePlayingDateEmptyDate(String date) {
        PlayingDateCreationData creationData = new PlayingDateCreationData(1L, 2, date);
        when(seasonService.getStageOfSeason(1L)).thenReturn(SeasonStage.PREPARATION);
        when(playingDateRepository.existsBySeasonIdAndNumber(1L, 2)).thenReturn(Boolean.FALSE);
        BadDataException actualException = assertThrows(BadDataException.class,
                () -> playingDateService.createPlayingDate(creationData));
        assertThat(actualException).hasMessage("Das Datum darf nicht leer sein!");
    }

    @Test
    void testCreatePlayingDateOk() {
        PlayingDateCreationData creationData = new PlayingDateCreationData(1L, 2, "1.1.2001");
        when(seasonService.getStageOfSeason(1L)).thenReturn(SeasonStage.PREPARATION);
        when(playingDateRepository.existsBySeasonIdAndNumber(1L, 2)).thenReturn(Boolean.FALSE);
        when(playingDateRepository.findBySeasonIdAndNumber(1L, 2))
                .thenReturn(Optional.of(playingDate));

        PlayingDateData actual = playingDateService.createPlayingDate(creationData);
        assertThat(actual.id()).isEqualTo(1L);
        assertThat(actual.seasonId()).isEqualTo(1L);
        assertThat(actual.number()).isEqualTo(2);
        assertThat(actual.date()).isEqualTo("1.1.2001");
    }

}