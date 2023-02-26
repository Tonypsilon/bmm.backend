package de.tonypsilon.bmm.backend.division.service;

import com.google.common.collect.SortedSetMultimap;
import de.tonypsilon.bmm.backend.division.data.Division;
import de.tonypsilon.bmm.backend.division.data.DivisionCreationData;
import de.tonypsilon.bmm.backend.division.data.DivisionData;
import de.tonypsilon.bmm.backend.division.data.DivisionRepository;
import de.tonypsilon.bmm.backend.exception.*;
import de.tonypsilon.bmm.backend.season.service.SeasonService;
import de.tonypsilon.bmm.backend.season.service.SeasonStage;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.Iterator;
import java.util.List;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

class DivisionServiceTest {

    private final DivisionRepository divisionRepository = mock(DivisionRepository.class);
    private final SeasonService seasonService = mock(SeasonService.class);
    private DivisionService divisionService;
    private Division landesliga, stadtligaA, stadtLigaB, divisionOfOtherSeason;
    private DivisionData landesligaData, stadtligaAData, stadtLigaBData;

    @BeforeEach
    void setUp() {
        this.divisionService = new DivisionService(divisionRepository, seasonService);
        this.landesligaData = new DivisionData(1L, "Landesliga", 1, 8, 1L);
        this.stadtligaAData = new DivisionData(2L, "Stadtliga A", 2, 8, 1L);
        this.stadtLigaBData = new DivisionData(3L, "Stadtliga B", 2, 8, 1L);
        this.landesliga = new Division();
        this.landesliga.setId(1L);
        this.landesliga.setName("Landesliga");
        this.landesliga.setLevel(1);
        this.landesliga.setNumberOfBoards(8);
        this.landesliga.setSeasonId(1L);
        this.stadtligaA = new Division();
        this.stadtligaA.setId(2L);
        this.stadtligaA.setName("Stadtliga A");
        this.stadtligaA.setLevel(2);
        this.stadtligaA.setNumberOfBoards(8);
        this.stadtligaA.setSeasonId(1L);
        this.stadtLigaB = new Division();
        this.stadtLigaB.setId(3L);
        this.stadtLigaB.setName("Stadtliga B");
        this.stadtLigaB.setLevel(2);
        this.stadtLigaB.setNumberOfBoards(8);
        this.stadtLigaB.setSeasonId(1L);
        this.divisionOfOtherSeason = new Division();
        this.divisionOfOtherSeason.setId(4L);
        this.divisionOfOtherSeason.setName("Landesliga");
        this.divisionOfOtherSeason.setLevel(1);
        this.divisionOfOtherSeason.setNumberOfBoards(8);
        this.divisionOfOtherSeason.setSeasonId(2L);
    }

    @Test
    void testGetAllDivisionsOfSeasonByLevel() {
        when(divisionRepository.findBySeasonId(1L)).thenReturn(List.of(
           landesliga, stadtligaA, stadtLigaB));
        SortedSetMultimap<Integer, DivisionData> actual = divisionService.getAllDivisionsOfSeasonByLevel(1L);
        assertThat(actual.size()).isEqualTo(3);

        assertThat(actual.get(1).size()).isEqualTo(1);
        Iterator<DivisionData> level1Actual = actual.get(1).iterator();
        assertThat(level1Actual.next()).isEqualTo(landesligaData);
        assertFalse(level1Actual.hasNext());

        assertThat(actual.get(2).size()).isEqualTo(2);
        Iterator<DivisionData> level2Actual = actual.get(2).iterator();
        assertThat(level2Actual.next()).isEqualTo(stadtligaAData);
        assertThat(level2Actual.next()).isEqualTo(stadtLigaBData);
        assertFalse(level2Actual.hasNext());
    }

    @Test
    void testGetSeasonIdByDivisionIdOk() {
        when(divisionRepository.findById(1L)).thenReturn(Optional.of(landesliga));
        Long actual = divisionService.getSeasonIdByDivisionId(1L);
        assertThat(actual).isEqualTo(landesliga.getSeasonId());
    }

    @Test
    void testGetSeasonIdByDivisionIdDivisionDoesNotExist() {
        when(divisionRepository.findById(-1L)).thenReturn(Optional.empty());
        NotFoundException actualException = assertThrows(NotFoundException.class,
                () -> divisionService.getSeasonIdByDivisionId(-1L));
        assertThat(actualException.getMessage())
                .isEqualTo("Es gibt keine Staffel mit der ID -1!");
    }

    @Test
    void testCreateDivisionOk() {
        when(divisionRepository.existsBySeasonIdAndName(1L, "Landesliga")).thenReturn(Boolean.FALSE);
        when(divisionRepository.getBySeasonIdAndName(1L, "Landesliga")).thenReturn(landesliga);
        when(seasonService.seasonExistsById(1L)).thenReturn(Boolean.TRUE);
        when(seasonService.getStageOfSeason(1L)).thenReturn(SeasonStage.PREPARATION);
        DivisionCreationData divisionCreationData = new DivisionCreationData("Landesliga", 1, 8, 1L);
        DivisionData actual = divisionService.createDivision(divisionCreationData);
        assertThat(actual).isEqualTo(landesligaData);
        verify(divisionRepository, times(1)).save(
                argThat(division -> division.getName().equals("Landesliga")
                && division.getLevel().equals(1)
                && division.getNumberOfBoards().equals(8)
                && division.getSeasonId().equals(1L)));
    }

    @Test
    void testCreateDivisionNullData() {
        NameBlankException nameBlankException = assertThrows(NameBlankException.class,
                () -> divisionService.createDivision(
                        new DivisionCreationData("",1,8,1L)));
        assertThat(nameBlankException.getMessage())
                .isEqualTo("Der Name der Staffel darf nicht leer sein!");
        NameBlankException nameNullException = assertThrows(NameBlankException.class,
                () -> divisionService.createDivision(
                        new DivisionCreationData(null,1,8,1L)));
        assertThat(nameNullException.getMessage())
                .isEqualTo("Der Name der Staffel darf nicht leer sein!");
        BadDataException seasonNullException = assertThrows(BadDataException.class,
                () -> divisionService.createDivision(
                        new DivisionCreationData("Landesliga", 1, 8 ,null)));
        assertThat(seasonNullException.getMessage())
                .isEqualTo("Zur Erstellung einer Staffel muss eine Saison gegeben sein!");
    }

    @Test
    void testCreateDivisionSeasonDoesNotExist() {
        when(seasonService.seasonExistsById(2L)).thenReturn(Boolean.FALSE);
        NotFoundException actualException = assertThrows(NotFoundException.class,
                () -> divisionService.createDivision(
                        new DivisionCreationData("Landesliga", 1, 8, 2L)
                )
        );
        assertThat(actualException.getMessage())
                .isEqualTo("Es gibt keine Saison mit der ID 2!");
    }

    @Test
    void testCreateDivisionWrongSeasonStage() {
        when(seasonService.seasonExistsById(1L)).thenReturn(Boolean.TRUE);
        when(seasonService.getStageOfSeason(1L)).thenReturn(SeasonStage.REGISTRATION);
        SeasonStageException actualException = assertThrows(SeasonStageException.class,
                () -> divisionService.createDivision(
                        new DivisionCreationData("Landesliga", 1, 8, 1L)
                )
        );
        assertThat(actualException.getMessage())
                .isEqualTo("Saison ist nicht in der Vorbereitungsphase!");
    }

    @Test
    void testCreateDivisionAlreadyExists() {
        when(seasonService.seasonExistsById(1L)).thenReturn(Boolean.TRUE);
        when(seasonService.getStageOfSeason(1L)).thenReturn(SeasonStage.PREPARATION);
        when(divisionRepository.existsBySeasonIdAndName(1L, "Landesliga")).thenReturn(Boolean.TRUE);
        AlreadyExistsException alreadyExistsException = assertThrows(AlreadyExistsException.class,
                () -> divisionService.createDivision(
                        new DivisionCreationData("Landesliga", 1, 8, 1L)));
        assertThat(alreadyExistsException.getMessage())
                .isEqualTo("Staffel mit Namen Landesliga für Saison mit ID 1 existiert bereits!");
    }

    @Test
    void testCreateDivisionInvalidNumberOfBoards() {
        when(divisionRepository.existsBySeasonIdAndName(1L, "Landesliga")).thenReturn(Boolean.FALSE);
        when(seasonService.seasonExistsById(1L)).thenReturn(Boolean.TRUE);
        when(seasonService.getStageOfSeason(1L)).thenReturn(SeasonStage.PREPARATION);

        DivisionCreationData divisionCreationDataNull = new DivisionCreationData("Landesliga", 1, null, 1L);
        DivisionCreationData divisionCreationDataInvalid = new DivisionCreationData("Landesliga", 1, 0, 1L);

        BadDataException actualExceptionNull = assertThrows(BadDataException.class,
                () -> divisionService.createDivision(divisionCreationDataNull));
        assertThat(actualExceptionNull.getMessage())
                .isEqualTo("Die Anzahl der Bretter für eine Staffel muss eine ganze Zahl > 0 sein!");

        BadDataException actualExceptionInvalid = assertThrows(BadDataException.class,
                () -> divisionService.createDivision(divisionCreationDataInvalid));
        assertThat(actualExceptionInvalid.getMessage())
                .isEqualTo("Die Anzahl der Bretter für eine Staffel muss eine ganze Zahl > 0 sein!");
    }

    @Test
    void testCreateDivisionInvalidLevel() {
        when(divisionRepository.existsBySeasonIdAndName(1L, "Landesliga")).thenReturn(Boolean.FALSE);
        when(seasonService.seasonExistsById(1L)).thenReturn(Boolean.TRUE);
        when(seasonService.getStageOfSeason(1L)).thenReturn(SeasonStage.PREPARATION);

        DivisionCreationData divisionCreationData = new DivisionCreationData("Landesliga", null, 8, 1L);

        BadDataException actualException = assertThrows(BadDataException.class,
                () -> divisionService.createDivision(divisionCreationData));
        assertThat(actualException.getMessage()).isEqualTo("Das Level der Staffel muss eine ganze Zahl > 0 sein!");
    }

    @Test
    void testDivisionExistsById() {
        when(divisionRepository.existsById(1L)).thenReturn(Boolean.TRUE);
        when(divisionRepository.existsById(2L)).thenReturn(Boolean.FALSE);
        assertThat(divisionService.divisionExistsById(1L)).isEqualTo(Boolean.TRUE);
        assertThat(divisionService.divisionExistsById(2L)).isEqualTo(Boolean.FALSE);
    }

    @Test
    void testGetNumberOfBoardsByDivisionIdOk() {
        when(divisionRepository.findById(1L)).thenReturn(Optional.of(landesliga));

        Integer actual = divisionService.getNumberOfBoardsByDivisionId(1L);
        assertThat(actual).isEqualTo(8);
    }

    @Test
    void testGetNumberOfBoardsByDivisionIdDoesNotExist() {
        when(divisionRepository.findById(-1L)).thenReturn(Optional.empty());

        NotFoundException actualException = assertThrows(NotFoundException.class,
                () -> divisionService.getNumberOfBoardsByDivisionId(-1L));
        assertThat(actualException.getMessage()).isEqualTo("Es gibt keine Staffel mit der ID -1!");
    }
}