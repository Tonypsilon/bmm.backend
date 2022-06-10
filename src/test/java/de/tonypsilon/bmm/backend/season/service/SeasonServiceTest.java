package de.tonypsilon.bmm.backend.season.service;

import de.tonypsilon.bmm.backend.exception.AlreadyExistsException;
import de.tonypsilon.bmm.backend.exception.NameBlankException;
import de.tonypsilon.bmm.backend.exception.NotFoundException;
import de.tonypsilon.bmm.backend.season.data.Season;
import de.tonypsilon.bmm.backend.season.data.SeasonData;
import de.tonypsilon.bmm.backend.season.data.SeasonRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.Collection;
import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

class SeasonServiceTest {

    private final SeasonRepository seasonRepository = mock(SeasonRepository.class);
    private SeasonService seasonService;
    Season seasonRegistration;
    Season seasonPreparation;
    Season seasonRunning;
    Season seasonCompleted;
    Season seasonArchived;
    private final SeasonData seasonRegistrationData = new SeasonData(1L, "Saison-Registration", SeasonStage.REGISTRATION);
    private final SeasonData seasonPreparationData = new SeasonData(2L, "Saison-Preparation", SeasonStage.PREPARATION);
    private final SeasonData seasonRunningData = new SeasonData(3L, "Saison-Running", SeasonStage.RUNNING);
    private final SeasonData seasonCompletedData = new SeasonData(4L, "Saison-Completed", SeasonStage.COMPLETED);
    private final SeasonData seasonArchivedData = new SeasonData(5L, "Saison-Archived", SeasonStage.ARCHIVED);

    @BeforeEach
    private void setUp() {
        this.seasonService = new SeasonService(seasonRepository);
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
        SeasonData actual = seasonService.createSeason("Saison-Registration");
        assertEquals(seasonRegistrationData, actual);
        verify(seasonRepository, times(1)).save(
                argThat(season -> season.getName().equals("Saison-Registration")
                && season.getStage().equals(SeasonStage.REGISTRATION)));
    }

    @Test
    void testCreateSeasonWhereSeasonNameAlreadyExists() {
        when(seasonRepository.existsByName("name")).thenReturn(Boolean.TRUE);
        AlreadyExistsException exception = assertThrows(AlreadyExistsException.class,
                () -> seasonService.createSeason("name"));
        assertEquals("Saison mit dem Namen name existiert bereits!", exception.getMessage());
    }

    @Test
    void testCreateSeasonWithEmptyName() {
        NameBlankException exceptionNameBlank = assertThrows(NameBlankException.class,
                () -> seasonService.createSeason(""));
        assertEquals("Der Name der Saison darf nicht leer sein!", exceptionNameBlank.getMessage());
        NameBlankException exceptionNameNull = assertThrows(NameBlankException.class,
                () -> seasonService.createSeason(null));
        assertEquals("Der Name der Saison darf nicht leer sein!", exceptionNameNull.getMessage());
    }

    @Test
    void testGetNonArchivedSeasonByNameOk() {
        when(seasonRepository.findByName("Saison-Running")).thenReturn(Optional.of(seasonRunning));
        assertEquals(seasonRunningData, seasonService.getNonArchivedSeasonByName("Saison-Running"));
    }

    @Test
    void testGetNonArchivedSeasonByNameThatDoesNotExistOrIsArchived() {
        when(seasonRepository.findByName("Saison-Archived")).thenReturn(Optional.of(seasonArchived));
        NotFoundException exceptionArchived = assertThrows(NotFoundException.class,
                () -> seasonService.getNonArchivedSeasonByName("Saison-Archived"));
        assertEquals("Nichtarchivierte Saison mit dem Namen Saison-Archived existiert nicht!", exceptionArchived.getMessage());
        when(seasonRepository.findByName("foo")).thenReturn(Optional.empty());
        NotFoundException exceptionNonExistent = assertThrows(NotFoundException.class,
                () -> seasonService.getNonArchivedSeasonByName("foo"));
        assertEquals("Nichtarchivierte Saison mit dem Namen foo existiert nicht!", exceptionNonExistent.getMessage());
    }

    @Test
    void testGetArchivedSeasonByNameOk() {
        when(seasonRepository.findByName("Saison-Archived")).thenReturn(Optional.of(seasonArchived));
        assertEquals(seasonArchivedData, seasonService.getArchivedSeasonByName("Saison-Archived"));
    }

    @Test
    void testGetArchivedSeasonByNameThatDoesNotExistOrIsNonArchived() {
        when(seasonRepository.findByName("Saison-Completed")).thenReturn(Optional.of(seasonCompleted));
        NotFoundException exceptionNonArchived = assertThrows(NotFoundException.class,
                () -> seasonService.getArchivedSeasonByName("Saison-Completed"));
        assertEquals("Archivierte Saison mit dem Namen Saison-Completed existiert nicht!", exceptionNonArchived.getMessage());
        when(seasonRepository.findByName("foo")).thenReturn(Optional.empty());
        NotFoundException exceptionNonExistent = assertThrows(NotFoundException.class,
                () -> seasonService.getArchivedSeasonByName("foo"));
        assertEquals("Archivierte Saison mit dem Namen foo existiert nicht!", exceptionNonExistent.getMessage());
    }

    @Test
    void testGetAllNonArchivedSeasons() {
        when(seasonRepository.findAll()).thenReturn(
                List.of(seasonRegistration, seasonPreparation, seasonRunning, seasonCompleted, seasonArchived));
        Collection<SeasonData> actual = seasonService.getAllNonArchivedSeasons();
        assertEquals(4, actual.size());
        assertTrue(actual.containsAll(List.of(seasonRegistrationData, seasonPreparationData, seasonCompletedData, seasonRunningData)));

    }

    @Test
    void testGetAllArchivedSeasons() {
        when(seasonRepository.findAll()).thenReturn(
                List.of(seasonRegistration, seasonPreparation, seasonRunning, seasonCompleted, seasonArchived));
        Collection<SeasonData> actual = seasonService.getAllArchivedSeasons();
        assertEquals(1, actual.size());
        assertTrue(actual.containsAll(List.of(seasonArchivedData)));
    }
}