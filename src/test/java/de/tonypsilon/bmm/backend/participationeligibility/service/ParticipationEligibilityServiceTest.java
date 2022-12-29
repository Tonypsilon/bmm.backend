package de.tonypsilon.bmm.backend.participationeligibility.service;

import de.tonypsilon.bmm.backend.club.service.ClubService;
import de.tonypsilon.bmm.backend.exception.AlreadyExistsException;
import de.tonypsilon.bmm.backend.exception.BadDataException;
import de.tonypsilon.bmm.backend.exception.NotFoundException;
import de.tonypsilon.bmm.backend.exception.SeasonStageException;
import de.tonypsilon.bmm.backend.participationeligibility.data.ParticipationEligibility;
import de.tonypsilon.bmm.backend.participationeligibility.data.ParticipationEligibilityCreationData;
import de.tonypsilon.bmm.backend.participationeligibility.data.ParticipationEligibilityData;
import de.tonypsilon.bmm.backend.participationeligibility.data.ParticipationEligibilityRepository;
import de.tonypsilon.bmm.backend.season.service.SeasonService;
import de.tonypsilon.bmm.backend.season.service.SeasonStage;
import de.tonypsilon.bmm.backend.validation.service.ValidationService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;
import org.junit.jupiter.params.provider.ValueSource;

import java.util.Collection;
import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

class ParticipationEligibilityServiceTest {

    private final ParticipationEligibilityRepository participationEligibilityRepository =
            mock(ParticipationEligibilityRepository.class);
    private final SeasonService seasonService = mock(SeasonService.class);
    private final ClubService clubService = mock(ClubService.class);
    private final ValidationService validationService = new ValidationService();
    private ParticipationEligibilityService participationEligibilityService;
    private final ParticipationEligibilityData participationEligibilityData1 =
            new ParticipationEligibilityData(1L, 2L, 3L, "Max", "Mustermann", 1, Optional.empty());
    private final ParticipationEligibilityData participationEligibilityData2 =
            new ParticipationEligibilityData(2L, 2L, 3L, "Erika", "Musterfrau", 2, Optional.empty());
    private final ParticipationEligibilityData participationEligibilityData4 =
            new ParticipationEligibilityData(4L, 2L, 4L, "Hal", "Lo", 1, Optional.empty());
    private ParticipationEligibility participationEligibility1, participationEligibility2,
            participationEligibility3, participationEligibility4;

    @BeforeEach
    void setUp() {
        participationEligibilityService = new ParticipationEligibilityService(
                participationEligibilityRepository,
                seasonService,
                clubService,
                validationService);
        participationEligibility1 = new ParticipationEligibility();
        participationEligibility1.setId(1L);
        participationEligibility1.setSeasonId(2L);
        participationEligibility1.setClubId(3L);
        participationEligibility1.setForename("Max");
        participationEligibility1.setSurname("Mustermann");
        participationEligibility1.setPkz(1);
        participationEligibility1.setDwz(null);
        participationEligibility2 = new ParticipationEligibility();
        participationEligibility2.setId(2L);
        participationEligibility2.setSeasonId(2L);
        participationEligibility2.setClubId(3L);
        participationEligibility2.setForename("Erika");
        participationEligibility2.setSurname("Musterfrau");
        participationEligibility2.setPkz(2);
        participationEligibility2.setDwz(null);
        participationEligibility3 = new ParticipationEligibility();
        participationEligibility3.setId(3L);
        participationEligibility3.setSeasonId(3L);
        participationEligibility3.setClubId(3L);
        participationEligibility3.setForename("Max");
        participationEligibility3.setSurname("Mustermann");
        participationEligibility3.setPkz(1);
        participationEligibility3.setDwz(null);
        participationEligibility4 = new ParticipationEligibility();
        participationEligibility4.setId(4L);
        participationEligibility4.setSeasonId(2L);
        participationEligibility4.setClubId(4L);
        participationEligibility4.setForename("Hal");
        participationEligibility4.setSurname("Lo");
        participationEligibility4.setPkz(1);
        participationEligibility4.setDwz(null);
    }

    @Test
    void testCreateParticipationEligibilityOk() {
        when(seasonService.seasonExistsById(2L)).thenReturn(Boolean.TRUE);
        when(seasonService.getStageOfSeason(2L)).thenReturn(SeasonStage.REGISTRATION);
        when(clubService.clubExistsById(3L)).thenReturn(Boolean.TRUE);
        when(participationEligibilityRepository.existsBySeasonIdAndClubIdAndPkz(2L, 3L, 1))
                .thenReturn(Boolean.FALSE);
        when(participationEligibilityRepository.getBySeasonIdAndClubIdAndPkz(2L, 3L, 1))
                .thenReturn(participationEligibility1);
        ParticipationEligibilityData actual = participationEligibilityService.createParticipationEligibility(
                new ParticipationEligibilityCreationData(2L, 3L, "Max", "Mustermann", 1, Optional.empty()));
        assertEquals(participationEligibilityData1, actual);
    }

    @Test
    void testCreateParticipationEligibilitySeasonDoesNotExist() {
        when(seasonService.seasonExistsById(1L)).thenReturn(Boolean.FALSE);
        NotFoundException actualException = assertThrows(NotFoundException.class,
                () -> participationEligibilityService.createParticipationEligibility(
                        new ParticipationEligibilityCreationData(1L, 3L, "Max", "Mustermann", 1, Optional.empty())
                )
        );
        assertEquals("Es gibt keine Saison mit der ID 1!", actualException.getMessage());
    }

    @ParameterizedTest
    @EnumSource(value = SeasonStage.class, mode = EnumSource.Mode.EXCLUDE, names = {"REGISTRATION"})
    void testCreateParticipationEligibilityWrongSeasonStage(SeasonStage seasonStage) {
        when(seasonService.seasonExistsById(2L)).thenReturn(Boolean.TRUE);
        when(seasonService.getStageOfSeason(2L)).thenReturn(seasonStage);
        ParticipationEligibilityCreationData participationEligibilityCreationData =
                new ParticipationEligibilityCreationData(2L, 1L, "Max", "Mustermann", 1, Optional.empty());

        SeasonStageException actualException = assertThrows(SeasonStageException.class,
                () -> participationEligibilityService.createParticipationEligibility(participationEligibilityCreationData));
        assertEquals("Die Saison ist nicht in der Registrierungsphase!", actualException.getMessage());
    }

    @Test
    void testCreateParticipationEligibilityClubDoesNotExist() {
        when(seasonService.seasonExistsById(2L)).thenReturn(Boolean.TRUE);
        when(seasonService.getStageOfSeason(2L)).thenReturn(SeasonStage.REGISTRATION);
        when(clubService.clubExistsById(1L)).thenReturn(Boolean.FALSE);
        NotFoundException actualException = assertThrows(NotFoundException.class,
                () -> participationEligibilityService.createParticipationEligibility(
                        new ParticipationEligibilityCreationData(2L, 1L, "Max", "Mustermann", 1, Optional.empty())
                )
        );
        assertEquals("Es gibt keinen Verein mit der ID 1!", actualException.getMessage());
    }

    @Test
    void testCreateParticipationEligibilityInvalidForename() {
        when(seasonService.seasonExistsById(2L)).thenReturn(Boolean.TRUE);
        when(seasonService.getStageOfSeason(2L)).thenReturn(SeasonStage.REGISTRATION);
        when(clubService.clubExistsById(3L)).thenReturn(Boolean.TRUE);
        BadDataException actualException = assertThrows(BadDataException.class,
                () -> participationEligibilityService.createParticipationEligibility(
                        new ParticipationEligibilityCreationData(2L, 3L, "", "Mustermann", 1, Optional.empty())
                )
        );
        assertEquals("Der Name darf nicht leer sein!", actualException.getMessage());
    }

    @Test
    void testCreateParticipationEligibilityInvalidSurname() {
        when(seasonService.seasonExistsById(2L)).thenReturn(Boolean.TRUE);
        when(seasonService.getStageOfSeason(2L)).thenReturn(SeasonStage.REGISTRATION);
        when(clubService.clubExistsById(3L)).thenReturn(Boolean.TRUE);
        BadDataException actualException = assertThrows(BadDataException.class,
                () -> participationEligibilityService.createParticipationEligibility(
                        new ParticipationEligibilityCreationData(2L, 3L, "Max", "", 1, Optional.empty())
                )
        );
        assertEquals("Der Name darf nicht leer sein!", actualException.getMessage());
    }

    @Test
    void testCreateParticipationEligibilityInvalidDwz() {
        when(seasonService.seasonExistsById(2L)).thenReturn(Boolean.TRUE);
        when(seasonService.getStageOfSeason(2L)).thenReturn(SeasonStage.REGISTRATION);
        when(clubService.clubExistsById(3L)).thenReturn(Boolean.TRUE);
        BadDataException actualException = assertThrows(BadDataException.class,
                () -> participationEligibilityService.createParticipationEligibility(
                        new ParticipationEligibilityCreationData(2L, 3L, "Max", "Mustermann", 1, Optional.of(-1))
                )
        );
        assertEquals("Das Rating muss positiv sein!", actualException.getMessage());
    }

    @Test
    void testCreateParticipationEligibilityAlreadyExists() {
        when(seasonService.seasonExistsById(2L)).thenReturn(Boolean.TRUE);
        when(seasonService.getStageOfSeason(2L)).thenReturn(SeasonStage.REGISTRATION);
        when(clubService.clubExistsById(3L)).thenReturn(Boolean.TRUE);
        when(participationEligibilityRepository.existsBySeasonIdAndClubIdAndPkz(2L, 3L, 1))
                .thenReturn(Boolean.TRUE);
        AlreadyExistsException actualException = assertThrows(AlreadyExistsException.class,
                () -> participationEligibilityService.createParticipationEligibility(
                        new ParticipationEligibilityCreationData(2L, 3L, "Max", "Mustermann", 1, Optional.empty()))
        );
        assertEquals("Es gibt bereits eine Spielberechtigung für die Spielernummer 1 " +
                "für den Club mit ID 3 und die Saison mit der ID 2!", actualException.getMessage());
    }

    @Test
    void testGetAllParticipationEligibilitiesForSeason() {
        when(participationEligibilityRepository.getBySeasonId(2L))
                .thenReturn(List.of(participationEligibility1, participationEligibility2, participationEligibility4));
        Collection<ParticipationEligibilityData> actual = participationEligibilityService
                .getAllParticipationEligibilitiesForSeason(2L);
        assertEquals(3, actual.size());
        assertTrue(actual.containsAll(List.of(participationEligibilityData1,
                participationEligibilityData2, participationEligibilityData4)));
    }

    @Test
    void testGetAllParticipationEligibilitiesForSeasonAndClub() {
        when(participationEligibilityRepository.getBySeasonIdAndClubId(2L, 3L))
                .thenReturn(List.of(participationEligibility1, participationEligibility2));
        Collection<ParticipationEligibilityData> actual = participationEligibilityService
                .getAllParticipationEligibilitiesForSeasonAndClub(2L, 3L);
        assertEquals(2, actual.size());
        assertTrue(actual.containsAll(List.of(participationEligibilityData1, participationEligibilityData2)));
    }

    @Test
    void testDeleteParticipationEligibilityOk() {
        when(participationEligibilityRepository.findById(4L))
                .thenReturn(Optional.of(participationEligibility4));
        participationEligibilityService.deleteParticipationEligibility(4L);
        verify(participationEligibilityRepository, times(1)).delete(
                argThat(participationEligibility -> participationEligibility.getId().equals(4L)
                && participationEligibility.getSeasonId().equals(2L)
                && participationEligibility.getClubId().equals(4L)
                && participationEligibility.getForename().equals("Hal")
                && participationEligibility.getSurname().equals("Lo")
                && participationEligibility.getPkz().equals(1)
                && participationEligibility.getDwz().isEmpty())
        );
    }

    @Test
    void testDeleteParticipationEligibilityThatDoesNotExist() {
        when(participationEligibilityRepository.findById(5L))
                .thenReturn(Optional.empty());
        NotFoundException actualException = assertThrows(NotFoundException.class,
                () -> participationEligibilityService.deleteParticipationEligibility(5L)
        );
        assertEquals("Es gibt keine Spielberechtigung mit der ID 5!", actualException.getMessage());
    }

    @ParameterizedTest
    @ValueSource(strings = { "true", "false" })
    void testExistsById(boolean doesExist) {
        when(participationEligibilityRepository.existsById(1L)).thenReturn(doesExist);
        assertEquals(doesExist, participationEligibilityService.existsById(1L));
    }
}