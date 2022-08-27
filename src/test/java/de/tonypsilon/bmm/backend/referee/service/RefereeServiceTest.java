package de.tonypsilon.bmm.backend.referee.service;

import de.tonypsilon.bmm.backend.exception.AlreadyExistsException;
import de.tonypsilon.bmm.backend.exception.BadDataException;
import de.tonypsilon.bmm.backend.exception.NotFoundException;
import de.tonypsilon.bmm.backend.referee.data.CreateRefereeData;
import de.tonypsilon.bmm.backend.referee.data.RefereeRepository;
import de.tonypsilon.bmm.backend.season.service.SeasonService;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class RefereeServiceTest {

    private final RefereeRepository refereeRepository = mock(RefereeRepository.class);
    private final SeasonService seasonService = mock(SeasonService.class);
    private RefereeService refereeService;

    @BeforeEach
    private void setUp() {
        refereeService = new RefereeService(refereeRepository, seasonService);
    }

    @Test
    void testCreateRefereeOk() {

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
                1L, "Forename", "", "for.sure@name.com");
        BadDataException actualException = assertThrows(BadDataException.class,
                () -> refereeService.createReferee(createRefereeData));
        assertEquals("Der Name darf nicht leer sein!", actualException.getMessage());
    }

    @Test
    void testCreateRefereeInvalidEmailAddress() {
        when(seasonService.seasonExistsById(1L)).thenReturn(Boolean.TRUE);
        when(refereeRepository.existsBySeasonIdAndEmailAddress(1L, "fore.sure@name.com")).thenReturn(Boolean.FALSE);
        CreateRefereeData createRefereeData = new CreateRefereeData(
                1L, "Forename", "Surname", "for.sure.name.com");
        BadDataException actualException = assertThrows(BadDataException.class,
                () -> refereeService.createReferee(createRefereeData));
        assertEquals("Die E-Mailadresse ist ungültig!", actualException.getMessage());
    }
}