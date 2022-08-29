package de.tonypsilon.bmm.backend.referee.service;

import de.tonypsilon.bmm.backend.exception.*;
import de.tonypsilon.bmm.backend.referee.data.*;
import de.tonypsilon.bmm.backend.season.service.SeasonService;
import de.tonypsilon.bmm.backend.season.service.SeasonStage;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;

import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.argThat;
import static org.mockito.Mockito.*;

class RefereeServiceTest {

    private final RefereeRepository refereeRepository = mock(RefereeRepository.class);
    private final SeasonService seasonService = mock(SeasonService.class);
    private RefereeService refereeService;
    private final RefereeData refereeData = new RefereeData(
            1L, 1L, "Forename", "Surname","fore.sure@name.com");
    private Referee referee1;

    @BeforeEach
    private void setUp() {
        refereeService = new RefereeService(refereeRepository, seasonService);
        referee1 = new Referee();
        referee1.setId(1L);
        referee1.setSeasonId(1L);
        referee1.setForename("Forename");
        referee1.setSurname("Surname");
        referee1.setEmailAddress("fore.sure@name.com");
    }

    @Test
    void testCreateRefereeOk() {
        when(seasonService.seasonExistsById(1L)).thenReturn(Boolean.TRUE);
        when(refereeRepository.existsBySeasonIdAndEmailAddress(1L, "fore.sure@name.com")).thenReturn(Boolean.FALSE);
        when(refereeRepository.getBySeasonIdAndEmailAddress(1L, "fore.sure@name.com"))
                .thenReturn(referee1);
        CreateRefereeData createRefereeData = new CreateRefereeData(
                1L, "Forename", "Surname", "fore.sure@name.com");
        RefereeData actual = refereeService.createReferee(createRefereeData);
        assertEquals(refereeData, actual);
        verify(refereeRepository, times(1)).save(argThat(
                referee -> referee.getSeasonId().equals(1L)
                        && referee.getForename().equals("Forename")
                        && referee.getSurname().equals("Surname")
                        && referee.getEmailAddress().equals("fore.sure@name.com")
        ));
    }

    @Test
    void testCreateRefereeSeasonDoesNotExist() {
        when(seasonService.seasonExistsById(1L)).thenReturn(Boolean.FALSE);
        CreateRefereeData createRefereeData = new CreateRefereeData(
                1L, "Forename", "Surname", "fore.sure@name.com");
        NotFoundException actualException = assertThrows(NotFoundException.class,
                () -> refereeService.createReferee(createRefereeData));
        assertEquals("Es gibt keine Saison mit der ID 1!", actualException.getMessage());
    }

    @Test
    void testCreateRefereeAlreadyExists() {
        when(seasonService.seasonExistsById(1L)).thenReturn(Boolean.TRUE);
        when(refereeRepository.existsBySeasonIdAndEmailAddress(1L, "fore.sure@name.com")).thenReturn(Boolean.TRUE);
        CreateRefereeData createRefereeData = new CreateRefereeData(
                1L, "Forename", "Surname", "fore.sure@name.com");
        AlreadyExistsException actualException = assertThrows(AlreadyExistsException.class,
                () -> refereeService.createReferee(createRefereeData));
        assertEquals(
                "Es gibt bereits einen Schiedsrichter für die Saison mit der ID 1 und der E-Mailadresse fore.sure@name.com!",
                actualException.getMessage());
    }

    @Test
    void testCreateRefereeInvalidForename() {
        when(seasonService.seasonExistsById(1L)).thenReturn(Boolean.TRUE);
        when(refereeRepository.existsBySeasonIdAndEmailAddress(1L, "fore.sure@name.com")).thenReturn(Boolean.FALSE);
        CreateRefereeData createRefereeData = new CreateRefereeData(
                1L, "", "Surname", "for.sure@name.com");
        BadDataException actualException = assertThrows(BadDataException.class,
                () -> refereeService.createReferee(createRefereeData));
        assertEquals("Der Name darf nicht leer sein!", actualException.getMessage());
    }

    @Test
    void testCreateRefereeInvalidSurname() {
        when(seasonService.seasonExistsById(1L)).thenReturn(Boolean.TRUE);
        when(refereeRepository.existsBySeasonIdAndEmailAddress(1L, "fore.sure@name.com")).thenReturn(Boolean.FALSE);
        CreateRefereeData createRefereeData = new CreateRefereeData(
                1L, "Forename", "", "fore.sure@name.com");
        BadDataException actualException = assertThrows(BadDataException.class,
                () -> refereeService.createReferee(createRefereeData));
        assertEquals("Der Name darf nicht leer sein!", actualException.getMessage());
    }

    @Test
    void testCreateRefereeInvalidEmailAddress() {
        when(seasonService.seasonExistsById(1L)).thenReturn(Boolean.TRUE);
        when(refereeRepository.existsBySeasonIdAndEmailAddress(1L, "fore.sure.name.com")).thenReturn(Boolean.FALSE);
        CreateRefereeData createRefereeData = new CreateRefereeData(
                1L, "Forename", "Surname", "fore.sure.name.com");
        BadDataException actualException = assertThrows(BadDataException.class,
                () -> refereeService.createReferee(createRefereeData));
        assertEquals("Die E-Mailadresse ist ungültig!", actualException.getMessage());
    }

    @Test
    void testUpdateRefereeOk() {
        when(refereeRepository.findById(1L)).thenReturn(Optional.of(referee1));
        when(refereeRepository.existsBySeasonIdAndEmailAddress(1L, "fore.sure2@name.com")).thenReturn(Boolean.FALSE);
        RefereeData refereeData = new RefereeData(1L, 1L, "Forename2", "Surname2", "fore.sure2@name.com");
        Referee updatedReferee = new Referee();
        updatedReferee.setId(1L);
        updatedReferee.setSeasonId(1L);
        updatedReferee.setForename("Forename2");
        updatedReferee.setSurname("Surname2");
        updatedReferee.setEmailAddress("fore.sure2@name.com");
        when(refereeRepository.getBySeasonIdAndEmailAddress(1L, "fore.sure2@name.com")).thenReturn(updatedReferee);
        RefereeData actual = refereeService.updateReferee(refereeData);
        assertEquals(refereeData, actual);
        verify(refereeRepository, times(1)).save(argThat(
                referee -> referee.getId().equals(1L)
                && referee.getSeasonId().equals(1L)
                && referee.getForename().equals("Forename2")
                && referee.getSurname().equals("Surname2")
                && referee.getEmailAddress().equals("fore.sure2@name.com")
        ));
    }

    @Test
    void testUpdateRefereeDoesNotExist() {
        when(refereeRepository.findById(2L)).thenReturn(Optional.empty());
        RefereeData refereeData = new RefereeData(2L, 1L, "Forename", "Surname", "fore.sure@name.com");
        NotFoundException actualException = assertThrows(NotFoundException.class,
                () -> refereeService.updateReferee(refereeData));
        assertEquals("Es gibt keinen Schiedsrichter mit der ID 2!", actualException.getMessage());
    }

    @Test
    void testUpdateRefereeSeasonChange() {
        when(refereeRepository.findById(1L)).thenReturn(Optional.of(referee1));
        RefereeData refereeData = new RefereeData(1L, 2L, "Forename", "Surname", "fore.sure@name.com");
        BadDataException actualException = assertThrows(BadDataException.class,
                () -> refereeService.updateReferee(refereeData));
        assertEquals("Die Saison darf sich nicht ändern!", actualException.getMessage());
    }

    @Test
    void testUpdateRefereeAlreadyExists() {
        when(refereeRepository.findById(1L)).thenReturn(Optional.of(referee1));
        when(refereeRepository.existsBySeasonIdAndEmailAddress(1L, "fore.sure2@name.com")).thenReturn(Boolean.TRUE);
        RefereeData refereeData = new RefereeData(1L, 1L, "Forename", "Surname", "fore.sure2@name.com");
        AlreadyExistsException actualException = assertThrows(AlreadyExistsException.class,
                () -> refereeService.updateReferee(refereeData));
        assertEquals(
                "Es gibt bereits einen Schiedsrichter für die Saison mit der ID 1 und der E-Mailadresse fore.sure2@name.com!",
                actualException.getMessage());
    }

    @Test
    void testUpdateRefereeInvalidForename() {
        when(refereeRepository.findById(1L)).thenReturn(Optional.of(referee1));
        when(refereeRepository.existsBySeasonIdAndEmailAddress(1L, "fore.sure@name.com")).thenReturn(Boolean.FALSE);
        RefereeData refereeData = new RefereeData(1L, 1L, null, "Surname", "fore.sure@name.com");
        BadDataException actualException = assertThrows(BadDataException.class,
                () -> refereeService.updateReferee(refereeData));
        assertEquals("Der Name darf nicht leer sein!", actualException.getMessage());
    }

    @Test
    void testUpdateRefereeInvalidSurname() {
        when(refereeRepository.findById(1L)).thenReturn(Optional.of(referee1));
        when(refereeRepository.existsBySeasonIdAndEmailAddress(1L, "fore.sure@name.com")).thenReturn(Boolean.FALSE);
        RefereeData refereeData = new RefereeData(1L, 1L, "Forename", null, "fore.sure@name.com");
        BadDataException actualException = assertThrows(BadDataException.class,
                () -> refereeService.updateReferee(refereeData));
        assertEquals("Der Name darf nicht leer sein!", actualException.getMessage());
    }

    @Test
    void testUpdateRefereeInvalidEmailAddress() {
        when(refereeRepository.findById(1L)).thenReturn(Optional.of(referee1));
        when(refereeRepository.existsBySeasonIdAndEmailAddress(1L, "abc;@@domain.com")).thenReturn(Boolean.FALSE);
        RefereeData refereeData = new RefereeData(1L, 1L, "Forename", "Surname", "abc;@@domain.com");
        BadDataException actualException = assertThrows(BadDataException.class,
                () -> refereeService.updateReferee(refereeData));
        assertEquals("Die E-Mailadresse ist ungültig!", actualException.getMessage());
    }

    @ParameterizedTest
    @EnumSource(value = SeasonStage.class, names = {"REGISTRATION","PREPARATION"})
    void testDeleteRefereeOk(SeasonStage stage) {
        when(refereeRepository.findById(1L)).thenReturn(Optional.of(referee1));
        when(seasonService.getStageOfSeason(1L)).thenReturn(stage);
        CreateRefereeData createRefereeData = new CreateRefereeData(
                1L, "Forename", "Surname", "fore.sure@name.com");
        refereeService.deleteReferee(refereeData);
        verify(refereeRepository, times(1)).delete(argThat(
                referee -> referee.getId().equals(1L)
                && referee.getSeasonId().equals(1L)
                && referee.getForename().equals("Forename")
                && referee.getSurname().equals("Surname")
                && referee.getEmailAddress().equals("fore.sure@name.com")
        ));
    }

    @Test
    void testDeleteRefereeDoesNotExist() {
        when(refereeRepository.findById(2L)).thenReturn(Optional.empty());
        RefereeData refereeData = new RefereeData(2L, 1L, "Forename", "Surname", "fore.sure@name.com");
        NotFoundException actualException = assertThrows(NotFoundException.class,
                () -> refereeService.deleteReferee(refereeData));
        assertEquals("Es gibt keinen Schiedsrichter mit der ID 2!", actualException.getMessage());
    }

    @ParameterizedTest
    @EnumSource(value = SeasonStage.class, names = {"RUNNING", "COMPLETED", "ARCHIVED"})
    void testDeleteRefereeWrongSeasonStage(SeasonStage stage) {
        when(refereeRepository.findById(1L)).thenReturn(Optional.of(referee1));
        when(seasonService.getStageOfSeason(1L)).thenReturn(stage);
        CreateRefereeData createRefereeData = new CreateRefereeData(
                1L, "Forename", "Surname", "fore.sure@name.com");
        SeasonStageException actualException = assertThrows(SeasonStageException.class,
                () -> refereeService.deleteReferee(refereeData));
        assertEquals("In dieser Saisonphase kann kein Schiedsrichter gelöscht werden!",
                actualException.getMessage());

    }

    @Test
    void testFindById() {
        when(refereeRepository.findById(1L)).thenReturn(Optional.of(referee1));
        when(refereeRepository.findById(2L)).thenReturn(Optional.empty());
        Optional<RefereeData> actual = refereeService.findById(1L);
        assertTrue(actual.isPresent());
        assertEquals(refereeData, actual.get());
        actual = refereeService.findById(2L);
        assertTrue(actual.isEmpty());
    }
}