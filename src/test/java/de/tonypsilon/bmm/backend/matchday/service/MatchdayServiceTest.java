package de.tonypsilon.bmm.backend.matchday.service;

import de.tonypsilon.bmm.backend.division.service.DivisionService;
import de.tonypsilon.bmm.backend.exception.AlreadyExistsException;
import de.tonypsilon.bmm.backend.exception.BadDataException;
import de.tonypsilon.bmm.backend.exception.NotFoundException;
import de.tonypsilon.bmm.backend.exception.SeasonStageException;
import de.tonypsilon.bmm.backend.matchday.data.CreateMatchdayData;
import de.tonypsilon.bmm.backend.matchday.data.Matchday;
import de.tonypsilon.bmm.backend.matchday.data.MatchdayData;
import de.tonypsilon.bmm.backend.matchday.data.MatchdayRepository;
import de.tonypsilon.bmm.backend.season.service.SeasonService;
import de.tonypsilon.bmm.backend.season.service.SeasonStage;
import org.checkerframework.checker.units.qual.C;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

class MatchdayServiceTest {

    private MatchdayService matchdayService;
    private final MatchdayRepository matchdayRepository = mock(MatchdayRepository.class);
    private final DivisionService divisionService = mock(DivisionService.class);
    private final SeasonService seasonService = mock(SeasonService.class);
    private Matchday matchday1, matchday2;
    private final MatchdayData matchdayData1 = new MatchdayData(1L, 1L, "11.1.2001", 1);
    private final MatchdayData matchdayData2 = new MatchdayData(2L, 1L, "22.2.2001-KW3", 2);

    @BeforeEach
    private void setUp() {
        matchdayService = new MatchdayService(matchdayRepository,
                divisionService,
                seasonService);
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
        assertEquals(matchdayData2, actual);
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
        assertEquals("Es gibt keine Staffel mit der ID 3!", actualException.getMessage());
    }

    @Test
    void testCreateMatchdayAlreadyExists() {
        when(divisionService.divisionExistsById(1L)).thenReturn(Boolean.TRUE);
        when(matchdayRepository.existsByDivisionIdAndRound(1L, 2)).thenReturn(Boolean.TRUE);
        AlreadyExistsException actualException = assertThrows(AlreadyExistsException.class,
                () -> matchdayService.createMatchday(new CreateMatchdayData(1L, "1.1.1111", 2))
        );
        assertEquals("Es gibt für die Staffel mit der ID 1 und Runde 2 schon einen Spieltag!",
                actualException.getMessage());
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
        assertEquals("Für die Staffel mit ID 1 sollte als nächstes Runde 2 erstellt werden, nicht 3!",
                actualException.getMessage());
    }

    @Test
    void testCreateMatchdayInvalidMatchdayDate1() {
        prepareMocksForInvalidDateCase();
        BadDataException actualException = assertThrows(BadDataException.class,
                () -> matchdayService.createMatchday(new CreateMatchdayData(1L, "abc;", 2))
        );
        assertEquals("Das Spieltagsdatum enthält ungültige Zeichen!",
                actualException.getMessage());
    }

    @Test
    void testCreateMatchdayInvalidMatchdayDate2() {
        prepareMocksForInvalidDateCase();
        BadDataException actualException = assertThrows(BadDataException.class,
                () -> matchdayService.createMatchday(new CreateMatchdayData(1L, "abc/", 2))
        );
        assertEquals("Das Spieltagsdatum enthält ungültige Zeichen!",
                actualException.getMessage());
    }

    @Test
    void testCreateMatchdayInvalidMatchdayDate3() {
        prepareMocksForInvalidDateCase();
        BadDataException actualException = assertThrows(BadDataException.class,
                () -> matchdayService.createMatchday(new CreateMatchdayData(1L, "abc&", 2))
        );
        assertEquals("Das Spieltagsdatum enthält ungültige Zeichen!",
                actualException.getMessage());
    }

    @Test
    void testCreateMatchdayBlankMatchdayDate() {
        prepareMocksForInvalidDateCase();
        BadDataException actualException = assertThrows(BadDataException.class,
                () -> matchdayService.createMatchday(new CreateMatchdayData(1L, "", 2))
        );
        assertEquals("Das Spieltagsdatum darf nicht leer sein!",
                actualException.getMessage());
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