package de.tonypsilon.bmm.backend.referee.service;

import de.tonypsilon.bmm.backend.exception.*;
import de.tonypsilon.bmm.backend.referee.data.*;
import de.tonypsilon.bmm.backend.season.service.SeasonService;
import de.tonypsilon.bmm.backend.season.service.SeasonStage;
import de.tonypsilon.bmm.backend.validation.service.ValidationService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;

import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.*;

class RefereeServiceTest {

    private final RefereeRepository refereeRepository = mock(RefereeRepository.class);
    private final SeasonService seasonService = mock(SeasonService.class);
    private final ValidationService validationService = new ValidationService();
    private RefereeService refereeService;
    private final RefereeData refereeData = new RefereeData(
            1L, 1L, "Forename", "Surname","fore.sure@name.com");
    private Referee referee1;

    @BeforeEach
    void setUp() {
        refereeService = new RefereeService(refereeRepository,
                seasonService,
                validationService);
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
        RefereeCreationData refereeCreationData = new RefereeCreationData(
                1L, "Forename", "Surname", "fore.sure@name.com");
        RefereeData actual = refereeService.createReferee(refereeCreationData);
        assertThat(actual).isEqualTo(refereeData);
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
        RefereeCreationData refereeCreationData = new RefereeCreationData(
                1L, "Forename", "Surname", "fore.sure@name.com");
        NotFoundException actualException = assertThrows(NotFoundException.class,
                () -> refereeService.createReferee(refereeCreationData));
        assertThat(actualException.getMessage())
                .isEqualTo("Es gibt keine Saison mit der ID 1!");
    }

    @Test
    void testCreateRefereeAlreadyExists() {
        when(seasonService.seasonExistsById(1L)).thenReturn(Boolean.TRUE);
        when(refereeRepository.existsBySeasonIdAndEmailAddress(1L, "fore.sure@name.com")).thenReturn(Boolean.TRUE);
        RefereeCreationData refereeCreationData = new RefereeCreationData(
                1L, "Forename", "Surname", "fore.sure@name.com");
        AlreadyExistsException actualException = assertThrows(AlreadyExistsException.class,
                () -> refereeService.createReferee(refereeCreationData));
        assertThat(actualException.getMessage())
                .isEqualTo("Es gibt bereits einen Schiedsrichter für die Saison mit der ID 1 und der E-Mailadresse fore.sure@name.com!");
    }

    @Test
    void testCreateRefereeInvalidForename() {
        when(seasonService.seasonExistsById(1L)).thenReturn(Boolean.TRUE);
        when(refereeRepository.existsBySeasonIdAndEmailAddress(1L, "fore.sure@name.com")).thenReturn(Boolean.FALSE);
        RefereeCreationData refereeCreationData = new RefereeCreationData(
                1L, "", "Surname", "for.sure@name.com");
        BadDataException actualException = assertThrows(BadDataException.class,
                () -> refereeService.createReferee(refereeCreationData));
        assertThat(actualException.getMessage())
               .isEqualTo("Der Name darf nicht leer sein!");
    }

    @Test
    void testCreateRefereeInvalidSurname() {
        when(seasonService.seasonExistsById(1L)).thenReturn(Boolean.TRUE);
        when(refereeRepository.existsBySeasonIdAndEmailAddress(1L, "fore.sure@name.com")).thenReturn(Boolean.FALSE);
        RefereeCreationData refereeCreationData = new RefereeCreationData(
                1L, "Forename", "", "fore.sure@name.com");
        BadDataException actualException = assertThrows(BadDataException.class,
                () -> refereeService.createReferee(refereeCreationData));
        assertThat(actualException.getMessage())
                .isEqualTo("Der Name darf nicht leer sein!");
    }

    @Test
    void testCreateRefereeInvalidEmailAddress() {
        when(seasonService.seasonExistsById(1L)).thenReturn(Boolean.TRUE);
        when(refereeRepository.existsBySeasonIdAndEmailAddress(1L, "fore.sure.name.com")).thenReturn(Boolean.FALSE);
        RefereeCreationData refereeCreationData = new RefereeCreationData(
                1L, "Forename", "Surname", "fore.sure.name.com");
        BadDataException actualException = assertThrows(BadDataException.class,
                () -> refereeService.createReferee(refereeCreationData));
        assertThat(actualException.getMessage())
                .isEqualTo("Die E-Mailadresse ist ungültig!");
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
        assertThat(actual).isEqualTo(refereeData);
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
        assertThat(actualException.getMessage())
                .isEqualTo("Es gibt keinen Schiedsrichter mit der ID 2!");
    }

    @Test
    void testUpdateRefereeSeasonChange() {
        when(refereeRepository.findById(1L)).thenReturn(Optional.of(referee1));
        RefereeData refereeData = new RefereeData(1L, 2L, "Forename", "Surname", "fore.sure@name.com");
        BadDataException actualException = assertThrows(BadDataException.class,
                () -> refereeService.updateReferee(refereeData));
        assertThat(actualException.getMessage())
                .isEqualTo("Die Saison darf sich nicht ändern!");
    }

    @Test
    void testUpdateRefereeAlreadyExists() {
        when(refereeRepository.findById(1L)).thenReturn(Optional.of(referee1));
        when(refereeRepository.existsBySeasonIdAndEmailAddress(1L, "fore.sure2@name.com")).thenReturn(Boolean.TRUE);
        RefereeData refereeData = new RefereeData(1L, 1L, "Forename", "Surname", "fore.sure2@name.com");
        AlreadyExistsException actualException = assertThrows(AlreadyExistsException.class,
                () -> refereeService.updateReferee(refereeData));
        assertThat(actualException.getMessage())
                .isEqualTo("Es gibt bereits einen Schiedsrichter für die Saison mit der ID 1 und der E-Mailadresse fore.sure2@name.com!");
    }

    @Test
    void testUpdateRefereeInvalidForename() {
        when(refereeRepository.findById(1L)).thenReturn(Optional.of(referee1));
        when(refereeRepository.existsBySeasonIdAndEmailAddress(1L, "fore.sure@name.com")).thenReturn(Boolean.FALSE);
        RefereeData refereeData = new RefereeData(1L, 1L, null, "Surname", "fore.sure@name.com");
        BadDataException actualException = assertThrows(BadDataException.class,
                () -> refereeService.updateReferee(refereeData));
        assertThat(actualException.getMessage())
                .isEqualTo("Der Name darf nicht leer sein!");
    }

    @Test
    void testUpdateRefereeInvalidSurname() {
        when(refereeRepository.findById(1L)).thenReturn(Optional.of(referee1));
        when(refereeRepository.existsBySeasonIdAndEmailAddress(1L, "fore.sure@name.com")).thenReturn(Boolean.FALSE);
        RefereeData refereeData = new RefereeData(1L, 1L, "Forename", null, "fore.sure@name.com");
        BadDataException actualException = assertThrows(BadDataException.class,
                () -> refereeService.updateReferee(refereeData));
        assertThat(actualException.getMessage())
                .isEqualTo("Der Name darf nicht leer sein!");
    }

    @Test
    void testUpdateRefereeInvalidEmailAddress() {
        when(refereeRepository.findById(1L)).thenReturn(Optional.of(referee1));
        when(refereeRepository.existsBySeasonIdAndEmailAddress(1L, "abc;@@domain.com")).thenReturn(Boolean.FALSE);
        RefereeData refereeData = new RefereeData(1L, 1L, "Forename", "Surname", "abc;@@domain.com");
        BadDataException actualException = assertThrows(BadDataException.class,
                () -> refereeService.updateReferee(refereeData));
        assertThat(actualException.getMessage())
                .isEqualTo("Die E-Mailadresse ist ungültig!");
    }

    @ParameterizedTest
    @EnumSource(value = SeasonStage.class, names = {"REGISTRATION","PREPARATION"})
    void testDeleteRefereeOk(SeasonStage stage) {
        when(refereeRepository.findById(1L)).thenReturn(Optional.of(referee1));
        when(seasonService.getStageOfSeason(1L)).thenReturn(stage);
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
        assertThat(actualException.getMessage())
                .isEqualTo("Es gibt keinen Schiedsrichter mit der ID 2!");
    }

    @ParameterizedTest
    @EnumSource(value = SeasonStage.class, names = {"RUNNING", "COMPLETED", "ARCHIVED"})
    void testDeleteRefereeWrongSeasonStage(SeasonStage stage) {
        when(refereeRepository.findById(1L)).thenReturn(Optional.of(referee1));
        when(seasonService.getStageOfSeason(1L)).thenReturn(stage);

        SeasonStageException actualException = assertThrows(SeasonStageException.class,
                () -> refereeService.deleteReferee(refereeData));
        assertThat(actualException.getMessage()).
                isEqualTo("In dieser Saisonphase kann kein Schiedsrichter gelöscht werden!");
    }

    @Test
    void testFindById() {
        when(refereeRepository.findById(1L)).thenReturn(Optional.of(referee1));
        when(refereeRepository.findById(2L)).thenReturn(Optional.empty());
        Optional<RefereeData> actual = refereeService.findById(1L);
        assertThat(actual).isPresent().hasValue(refereeData);
        actual = refereeService.findById(2L);
        assertTrue(actual.isEmpty());
    }
}