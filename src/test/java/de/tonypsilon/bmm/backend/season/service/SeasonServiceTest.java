package de.tonypsilon.bmm.backend.season.service;

import de.tonypsilon.bmm.backend.exception.AlreadyExistsException;
import de.tonypsilon.bmm.backend.exception.BadDataException;
import de.tonypsilon.bmm.backend.exception.NameBlankException;
import de.tonypsilon.bmm.backend.exception.NotFoundException;
import de.tonypsilon.bmm.backend.season.data.*;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.Collection;
import java.util.List;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

class SeasonServiceTest {

    private final SeasonRepository seasonRepository = mock(SeasonRepository.class);
    private SeasonService seasonService;
    private Season seasonRegistration;
    private Season seasonPreparation;
    private Season seasonRunning;
    private Season seasonCompleted;
    private Season seasonArchived;
    private final SeasonData seasonRegistrationData = new SeasonData(1L, "Saison-Registration", SeasonStage.REGISTRATION);
    private final SeasonData seasonPreparationData = new SeasonData(2L, "Saison-Preparation", SeasonStage.PREPARATION);
    private final SeasonData seasonRunningData = new SeasonData(3L, "Saison-Running", SeasonStage.RUNNING);
    private final SeasonData seasonCompletedData = new SeasonData(4L, "Saison-Completed", SeasonStage.COMPLETED);
    private final SeasonData seasonArchivedData = new SeasonData(5L, "Saison-Archived", SeasonStage.ARCHIVED);

    @BeforeEach
    void setUp() {
        seasonService = new SeasonService(seasonRepository);
        seasonRegistration = new Season();
        seasonRegistration.setId(1L);
        seasonRegistration.setName("Saison-Registration");
        seasonRegistration.setStage(SeasonStage.REGISTRATION);
        seasonPreparation = new Season();
        seasonPreparation.setId(2L);
        seasonPreparation.setName("Saison-Preparation");
        seasonPreparation.setStage(SeasonStage.PREPARATION);
        seasonRunning = new Season();
        seasonRunning.setId(3L);
        seasonRunning.setName("Saison-Running");
        seasonRunning.setStage(SeasonStage.RUNNING);
        seasonCompleted = new Season();
        seasonCompleted.setId(4L);
        seasonCompleted.setName("Saison-Completed");
        seasonCompleted.setStage(SeasonStage.COMPLETED);
        seasonArchived = new Season();
        seasonArchived.setId(5L);
        seasonArchived.setName("Saison-Archived");
        seasonArchived.setStage(SeasonStage.ARCHIVED);
    }

    @Test
    void testCreateSeasonOk() {
        when(seasonRepository.existsByName("Saison-Registration")).thenReturn(Boolean.FALSE);
        when(seasonRepository.getByName("Saison-Registration")).thenReturn(seasonRegistration);
        SeasonData actual = seasonService.createSeason(new SeasonCreationData("Saison-Registration"));
        assertThat(actual).isEqualTo(seasonRegistrationData);
        verify(seasonRepository, times(1)).save(
                argThat(season -> season.getName().equals("Saison-Registration")
                && season.getStage().equals(SeasonStage.REGISTRATION)));
    }

    @Test
    void testCreateSeasonWhereSeasonNameAlreadyExists() {
        when(seasonRepository.existsByName("name")).thenReturn(Boolean.TRUE);
        AlreadyExistsException exception = assertThrows(AlreadyExistsException.class,
                () -> seasonService.createSeason(new SeasonCreationData("name")));
        assertThat(exception.getMessage())
            .isEqualTo("Saison mit dem Namen name existiert bereits!");
    }

    @Test
    void testCreateSeasonWithEmptyName() {
        NameBlankException exceptionNameBlank = assertThrows(NameBlankException.class,
                () -> seasonService.createSeason(new SeasonCreationData("")));
        assertThat(exceptionNameBlank.getMessage())
            .isEqualTo("Der Name der Saison darf nicht leer sein!");
        NameBlankException exceptionNameNull = assertThrows(NameBlankException.class,
                () -> seasonService.createSeason(new SeasonCreationData(null)));
        assertThat(exceptionNameNull.getMessage())
            .isEqualTo("Der Name der Saison darf nicht leer sein!");
    }

    @Test
    void testGetSeasonByNameOk() {
        when(seasonRepository.findByName("Saison-Running")).thenReturn(Optional.of(seasonRunning));
        assertThat(seasonService.getSeasonByName("Saison-Running")).isEqualTo(seasonRunningData);
    }

    @Test
    void testGetSeasonByNameThatDoesNotExist() {
        when(seasonRepository.findByName("foo")).thenReturn(Optional.empty());
        NotFoundException exceptionNonExistent = assertThrows(NotFoundException.class,
                () -> seasonService.getSeasonByName("foo"));
        assertThat(exceptionNonExistent.getMessage())
            .isEqualTo("Saison mit dem Namen foo existiert nicht!");
    }

    @Test
    void testGetSeasonByIdOk() {
        when(seasonRepository.findById(1L)).thenReturn(Optional.of(seasonPreparation));
        assertThat(seasonService.getSeasonById(1L)).isEqualTo(seasonPreparationData);
    }

    @Test
    void testGetSeasonByIdThatDoesNotExist() {
        when(seasonRepository.findById(-1L)).thenReturn(Optional.empty());
        NotFoundException exceptionNonExistent = assertThrows(NotFoundException.class,
                () -> seasonService.getSeasonById(-1L));
        assertThat(exceptionNonExistent.getMessage())
            .isEqualTo("Saison mit der ID -1 existiert nicht!");
    }

    @Test
    void testGetAllSeasons() {
        when(seasonRepository.findAll()).thenReturn(
                List.of(seasonRegistration, seasonPreparation, seasonRunning, seasonCompleted, seasonArchived));
        Collection<SeasonData> actual = seasonService.getAllSeasons();
        assertThat(actual).hasSize(5);
        assertTrue(actual.containsAll(List.of(seasonRegistrationData, seasonPreparationData, seasonCompletedData, seasonRunningData, seasonArchivedData)));

    }

    @Test
    void testValidSeasonStageUpdate() {
        when(seasonRepository.findByName("Saison-Running")).thenReturn(Optional.of(seasonRunning));
        when(seasonRepository.getByName("Saison-Running")).thenReturn(seasonRunning);
        SeasonData actual = seasonService.updateSeasonStage(new SeasonStageChangeData("Saison-Running", SeasonStage.COMPLETED));
        assertThat(actual.name()).isEqualTo("Saison-Running");
        assertThat(actual.stage()).isEqualTo(SeasonStage.COMPLETED);
        verify(seasonRepository, times(1)).save(
                argThat(season -> season.getName().equals("Saison-Running")
                        && season.getStage().equals(SeasonStage.COMPLETED)));
    }

    @Test
    void testInvalidSeasonStageUpdate() {
        when(seasonRepository.findByName("Saison-Running")).thenReturn(Optional.of(seasonRunning));
        BadDataException actualException = assertThrows(BadDataException.class,
                () -> seasonService.updateSeasonStage(new SeasonStageChangeData("Saison-Running", SeasonStage.PREPARATION)));
        assertThat(actualException.getMessage())
            .isEqualTo("Eine Saison im Status RUNNING kann nicht in Status PREPARATION geändert werden!");
    }

    @Test
    void testUpdateSeasonStageWhereSeasonDoesNotExist() {
        when(seasonRepository.findByName("foo")).thenReturn(Optional.empty());
        NotFoundException actualException = assertThrows(NotFoundException.class,
                () -> seasonService.updateSeasonStage(new SeasonStageChangeData("foo", SeasonStage.COMPLETED)));
        assertThat(actualException.getMessage())
            .isEqualTo("Saison mit dem Namen foo existiert nicht!");
    }

    @Test
    void testSeasonExistsById() {
        when(seasonRepository.existsById(1L)).thenReturn(Boolean.TRUE);
        when(seasonRepository.existsById(-1L)).thenReturn(Boolean.FALSE);
        assertTrue(seasonService.seasonExistsById(1L));
        assertFalse(seasonService.seasonExistsById(-1L));
    }

    @Test
    void testGetStageOfSeason() {
        when(seasonRepository.findById(3L)).thenReturn(Optional.of(seasonRunning));
        assertThat(seasonService.getStageOfSeason(3L)).isEqualTo(SeasonStage.RUNNING);
    }

    @Test
    void testGetStageOfSeasonWhereSeasonDoesNotExist() {
        when(seasonRepository.findById(-1L)).thenReturn(Optional.empty());
        NotFoundException actualException = assertThrows(NotFoundException.class,
                () -> seasonService.getStageOfSeason(-1L));
        assertThat(actualException.getMessage())
            .isEqualTo("Saison mit ID -1 existiert nicht!");
    }
}