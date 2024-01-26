package de.tonypsilon.bmm.backend.matchday.service;

import de.tonypsilon.bmm.backend.division.service.DivisionService;
import de.tonypsilon.bmm.backend.exception.*;
import de.tonypsilon.bmm.backend.matchday.data.*;
import de.tonypsilon.bmm.backend.season.service.SeasonService;
import de.tonypsilon.bmm.backend.season.service.SeasonStage;
import de.tonypsilon.bmm.backend.validation.service.ValidationService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.*;

class MatchdayServiceTest {

    private MatchdayService matchdayService;
    private final MatchdayRepository matchdayRepository = mock(MatchdayRepository.class);
    private final DivisionService divisionService = mock(DivisionService.class);
    private final SeasonService seasonService = mock(SeasonService.class);
    private final ValidationService validationService = new ValidationService();
    private Matchday matchday1, matchday2;
    private final MatchdayData matchdayData1 = new MatchdayData(1L, 1L, "11.1.2001", 1);
    private final MatchdayData matchdayData2 = new MatchdayData(2L, 1L, "22.2.2001-KW3", 2);

    @BeforeEach
    void setUp() {
        matchdayService = new MatchdayService(matchdayRepository,
                divisionService,
                seasonService,
                validationService);
        matchday1 = new Matchday();
        matchday1.setId(1L);
        matchday1.setDivisionId(1L);
        matchday1.setDate("11.1.2001");
        matchday1.setRound(1);
        matchday2 = new Matchday();
        matchday2.setId(2L);
        matchday2.setDivisionId(1L);
        matchday2.setDate("22.2.2001-KW3");
        matchday2.setRound(2);
    }

    @Test
    void testCreateMatchdayOk() {
        when(divisionService.divisionExistsById(1L)).thenReturn(Boolean.TRUE);
        when(matchdayRepository.existsByDivisionIdAndRound(1L, 2)).thenReturn(Boolean.FALSE);
        when(divisionService.getSeasonIdByDivisionId(1L)).thenReturn(2L);
        when(seasonService.getStageOfSeason(2L)).thenReturn(SeasonStage.PREPARATION);
        when(matchdayRepository.findByDivisionIdOrderByRoundAsc(1L)).thenReturn(List.of(matchday1));
        when(matchdayRepository.getByDivisionIdAndRound(1L, 2)).thenReturn(matchday2);

        MatchdayData actual = matchdayService.createMatchday(new CreateMatchdayData(1L, "22.2.2001-KW3", 2));
        assertThat(actual).isEqualTo(matchdayData2);
        verify(matchdayRepository, times(1)).save(
                argThat(matchday -> matchday.getDivisionId().equals(1L)
                && matchday.getRound().equals(2)
                && matchday.getDate().equals("22.2.2001-KW3"))
        );
    }

    @Test
    void testCreateMatchdayDivisionDoesNotExist() {
        when(divisionService.divisionExistsById(3L)).thenReturn(Boolean.FALSE);
        NotFoundException actualException = assertThrows(NotFoundException.class,
                () -> matchdayService.createMatchday(new CreateMatchdayData(3L, "1.1.1111", 1))
        );
        assertThat(actualException.getMessage())
                .isEqualTo("Es gibt keine Staffel mit der ID 3!");
    }

    @Test
    void testCreateMatchdayAlreadyExists() {
        when(divisionService.divisionExistsById(1L)).thenReturn(Boolean.TRUE);
        when(matchdayRepository.existsByDivisionIdAndRound(1L, 2)).thenReturn(Boolean.TRUE);
        AlreadyExistsException actualException = assertThrows(AlreadyExistsException.class,
                () -> matchdayService.createMatchday(new CreateMatchdayData(1L, "1.1.1111", 2))
        );
        assertThat(actualException.getMessage())
                .isEqualTo("Es gibt für die Staffel mit der ID 1 und Runde 2 schon einen Spieltag!");
    }

    @Test
    void testCreateMatchdayWrongSeasonStage() {
        when(divisionService.divisionExistsById(1L)).thenReturn(Boolean.TRUE);
        when(matchdayRepository.existsByDivisionIdAndRound(1L, 2)).thenReturn(Boolean.FALSE);
        when(divisionService.getSeasonIdByDivisionId(1L)).thenReturn(2L);
        when(seasonService.getStageOfSeason(2L)).thenReturn(SeasonStage.RUNNING);
        SeasonStageException actualException = assertThrows(SeasonStageException.class,
                () -> matchdayService.createMatchday(new CreateMatchdayData(1L, "1.1.1111", 2))
        );
        assertThat(actualException.getMessage())
                .isEqualTo("Saison ist nicht in der Vorbereitungsphase!");
    }

    @Test
    void testCreateMatchdayInvalidRoundNumber() {
        when(divisionService.divisionExistsById(1L)).thenReturn(Boolean.TRUE);
        when(matchdayRepository.existsByDivisionIdAndRound(1L, 3)).thenReturn(Boolean.FALSE);
        when(divisionService.getSeasonIdByDivisionId(1L)).thenReturn(2L);
        when(seasonService.getStageOfSeason(2L)).thenReturn(SeasonStage.PREPARATION);
        when(matchdayRepository.findByDivisionIdOrderByRoundAsc(1L)).thenReturn(List.of(matchday1));

        BadDataException actualException = assertThrows(BadDataException.class,
                () -> matchdayService.createMatchday(new CreateMatchdayData(1L, "2.2.2", 3))
        );
        assertThat(actualException.getMessage())
                .isEqualTo("Für die Staffel mit ID 1 sollte als nächstes Runde 2 erstellt werden, nicht 3!");
    }

    @Test
    void testCreateMatchdayInvalidMatchdayDate1() {
        prepareMocksForInvalidDateCase();
        BadDataException actualException = assertThrows(BadDataException.class,
                () -> matchdayService.createMatchday(new CreateMatchdayData(1L, "abc;", 2))
        );
        assertThat(actualException.getMessage())
                .isEqualTo("Das Datum enthält ungültige Zeichen!");
    }

    @Test
    void testCreateMatchdayInvalidMatchdayDate2() {
        prepareMocksForInvalidDateCase();
        BadDataException actualException = assertThrows(BadDataException.class,
                () -> matchdayService.createMatchday(new CreateMatchdayData(1L, "abc/", 2))
        );
        assertThat(actualException.getMessage())
                .isEqualTo("Das Datum enthält ungültige Zeichen!");
    }

    @Test
    void testCreateMatchdayInvalidMatchdayDate3() {
        prepareMocksForInvalidDateCase();
        BadDataException actualException = assertThrows(BadDataException.class,
                () -> matchdayService.createMatchday(new CreateMatchdayData(1L, "abc&", 2))
        );
        assertThat(actualException.getMessage())
                .isEqualTo("Das Datum enthält ungültige Zeichen!");
    }

    @Test
    void testCreateMatchdayBlankMatchdayDate() {
        prepareMocksForInvalidDateCase();
        BadDataException actualException = assertThrows(BadDataException.class,
                () -> matchdayService.createMatchday(new CreateMatchdayData(1L, "", 2))
        );
        assertThat(actualException.getMessage())
                .isEqualTo("Das Datum darf nicht leer sein!");
    }

    @Test
    void testGetMatchdaysOfDivisionOrderedByRound() {
        when(matchdayRepository.findByDivisionIdOrderByRoundAsc(1L)).thenReturn(List.of(matchday1, matchday2));
        List<MatchdayData> actual = matchdayService.getMatchdaysOfDivisionOrderedByRound(1L);
        assertThat(actual).hasSize(2);
        assertTrue(actual.containsAll(List.of(matchdayData1, matchdayData2)));
    }

    @Test
    void testGetById() {
        when(matchdayRepository.findById(1L)).thenReturn(Optional.of(matchday1));
        when(matchdayRepository.findById(-1L)).thenReturn(Optional.empty());
        assertThat(matchdayService.getMatchdayDataById(1L)).isEqualTo(matchdayData1);
        NotFoundException notFoundException = assertThrows(NotFoundException.class,
                () -> matchdayService.getMatchdayDataById(-1L));
        assertThat(notFoundException.getMessage())
                .isEqualTo("Es gibt keinen Spieltag mit der ID -1!");
    }

    @Test
    void testUpdateMatchdayOk() {
        when(matchdayRepository.findById(1L)).thenReturn(Optional.of(matchday1));
        when(divisionService.getSeasonIdByDivisionId(1L)).thenReturn(2L);
        when(seasonService.getStageOfSeason(2L)).thenReturn(SeasonStage.RUNNING);
        Matchday modifiedMatchday = new Matchday();
        modifiedMatchday.setId(matchday1.getId());
        modifiedMatchday.setDivisionId(matchday1.getDivisionId());
        modifiedMatchday.setRound(matchday1.getRound());
        modifiedMatchday.setDate("12.1.2001");
        when(matchdayRepository.getByDivisionIdAndRound(1L, 1)).thenReturn(modifiedMatchday);
        MatchdayData actual = matchdayService.updateMatchday(new MatchdayData(1L, 1L, "12.1.2001", 1));
        assertThat(actual).isEqualTo(new MatchdayData(1L, 1L, "12.1.2001", 1));
        verify(matchdayRepository, times(1)).save(argThat(
                matchday -> matchday.getId().equals(1L)
                && matchday.getDivisionId().equals(1L)
                && matchday.getRound().equals(1)
                && matchday.getDate().equals("12.1.2001")
        ));
    }

    @Test
    void testUpdateMatchdayThatDoesNotExist() {
        when(matchdayRepository.findById(1L)).thenReturn(Optional.empty());
        NotFoundException actualException = assertThrows(NotFoundException.class,
                () -> matchdayService.updateMatchday(matchdayData1));
        assertThat(actualException.getMessage())
                .isEqualTo("Es gibt keinen Spieltag mit der ID 1!");
    }

    @Test
    void testUpdateMatchdayChangedDivision() {
        when(matchdayRepository.findById(1L)).thenReturn(Optional.of(matchday1));
        BadDataException actualException = assertThrows(BadDataException.class,
                () -> matchdayService.updateMatchday(new MatchdayData(1L, 2L, "13.1.2001", 1))
        );
        assertThat(actualException.getMessage())
                .isEqualTo("Die Staffel eines Spieltags kann sich nicht ändern!");
    }

    @Test
    void testUpdateMatchdayChangedRound() {
        when(matchdayRepository.findById(1L)).thenReturn(Optional.of(matchday1));
        BadDataException actualException = assertThrows(BadDataException.class,
                () -> matchdayService.updateMatchday(new MatchdayData(1L, 1L, "13.1.2001", 2))
        );
        assertThat(actualException.getMessage())
                .isEqualTo("Die Runde eines Spieltags kann sich nicht ändern!");
    }

    @Test
    void testUpdateMatchdayWrongSeasonStage() {
        when(matchdayRepository.findById(1L)).thenReturn(Optional.of(matchday1));
        when(divisionService.getSeasonIdByDivisionId(1L)).thenReturn(2L);
        when(seasonService.getStageOfSeason(2L)).thenReturn(SeasonStage.COMPLETED);
        SeasonStageException actualException = assertThrows(SeasonStageException.class,
                () -> matchdayService.updateMatchday(new MatchdayData(1L, 1L, "13.1.2001", 1))
        );
        assertThat(actualException.getMessage())
                .isEqualTo("In dieser Saisonphase können Spieltage nicht angepasst werden!");
    }

    @Test
    void testUpdateMatchdayInvalidDate() {
        when(matchdayRepository.findById(1L)).thenReturn(Optional.of(matchday1));
        when(divisionService.getSeasonIdByDivisionId(1L)).thenReturn(2L);
        when(seasonService.getStageOfSeason(2L)).thenReturn(SeasonStage.RUNNING);
        BadDataException actualException = assertThrows(BadDataException.class,
                () -> matchdayService.updateMatchday(new MatchdayData(1L, 1L, "123abc;", 1))
        );
        assertThat(actualException.getMessage())
                .isEqualTo("Das Datum enthält ungültige Zeichen!");
    }

    @Test
    void testDeleteMatchdayOk() {
        when(matchdayRepository.findById(2L)).thenReturn(Optional.of(matchday2));
        when(divisionService.getSeasonIdByDivisionId(1L)).thenReturn(2L);
        when(seasonService.getStageOfSeason(2L)).thenReturn(SeasonStage.PREPARATION);
        when(matchdayRepository.findByDivisionIdOrderByRoundAsc(1L)).thenReturn(List.of(matchday1, matchday2));
        matchdayService.deleteMatchday(2L);
        verify(matchdayRepository, times(1)).delete(argThat(
                matchday -> matchday.getId().equals(2L)
                && matchday.getDivisionId().equals(1L)
                && matchday.getRound().equals(2)
                && matchday.getDate().equals("22.2.2001-KW3"))
        );
    }

    @Test
    void testDeleteMatchdayThatDoesNotExist() {
        when(matchdayRepository.findById(2L)).thenReturn(Optional.empty());
        NotFoundException actualException = assertThrows(NotFoundException.class,
                () -> matchdayService.deleteMatchday(2L)
        );
        assertThat(actualException.getMessage())
                .isEqualTo("Es gibt keinen Spieltag mit der ID 2!");
    }

    @Test
    void testDeleteMatchdayWrongSeasonStage() {
        when(matchdayRepository.findById(2L)).thenReturn(Optional.of(matchday2));
        when(divisionService.getSeasonIdByDivisionId(1L)).thenReturn(2L);
        when(seasonService.getStageOfSeason(2L)).thenReturn(SeasonStage.ARCHIVED);
        SeasonStageException actualException = assertThrows(SeasonStageException.class,
                () -> matchdayService.deleteMatchday(2L)
        );
        assertThat(actualException.getMessage())
                .isEqualTo("In dieser Saisonphase können Spieltage nicht gelöscht werden!");
    }

    @Test
    void testDeleteMatchdayWrongRoundNumber() {
        when(matchdayRepository.findById(1L)).thenReturn(Optional.of(matchday1));
        when(divisionService.getSeasonIdByDivisionId(1L)).thenReturn(2L);
        when(seasonService.getStageOfSeason(2L)).thenReturn(SeasonStage.PREPARATION);
        when(matchdayRepository.findByDivisionIdOrderByRoundAsc(1L)).thenReturn(List.of(matchday1, matchday2));
        BadDataException actualException = assertThrows(BadDataException.class,
                () -> matchdayService.deleteMatchday(1L)
        );
        assertThat(actualException.getMessage())
                .isEqualTo("Für die Staffel mit ID 1 kann nur Runde 2 gelöscht werden, nicht 1!");
    }

    private void prepareMocksForInvalidDateCase() {
        when(divisionService.divisionExistsById(1L)).thenReturn(Boolean.TRUE);
        when(matchdayRepository.existsByDivisionIdAndRound(1L, 2)).thenReturn(Boolean.FALSE);
        when(divisionService.getSeasonIdByDivisionId(1L)).thenReturn(2L);
        when(seasonService.getStageOfSeason(2L)).thenReturn(SeasonStage.PREPARATION);
        when(matchdayRepository.findByDivisionIdOrderByRoundAsc(1L)).thenReturn(List.of(matchday1));
        when(matchdayRepository.getByDivisionIdAndRound(1L, 2)).thenReturn(matchday2);
    }



}