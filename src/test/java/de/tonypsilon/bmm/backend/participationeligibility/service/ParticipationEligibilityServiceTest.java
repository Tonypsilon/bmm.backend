package de.tonypsilon.bmm.backend.participationeligibility.service;

import de.tonypsilon.bmm.backend.club.service.ClubService;
import de.tonypsilon.bmm.backend.exception.AlreadyExistsException;
import de.tonypsilon.bmm.backend.exception.BadDataException;
import de.tonypsilon.bmm.backend.exception.NotFoundException;
import de.tonypsilon.bmm.backend.participationeligibility.data.ParticipationEligibility;
import de.tonypsilon.bmm.backend.participationeligibility.data.ParticipationEligibilityCreationData;
import de.tonypsilon.bmm.backend.participationeligibility.data.ParticipationEligibilityData;
import de.tonypsilon.bmm.backend.participationeligibility.data.ParticipationEligibilityRepository;
import de.tonypsilon.bmm.backend.season.service.SeasonService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class ParticipationEligibilityServiceTest {

    private final ParticipationEligibilityRepository participationEligibilityRepository =
            mock(ParticipationEligibilityRepository.class);
    private final SeasonService seasonService = mock(SeasonService.class);
    private final ClubService clubService = mock(ClubService.class);
    private ParticipationEligibilityService participationEligibilityService;
    private ParticipationEligibilityData participationEligibilityData1 =
            new ParticipationEligibilityData(1L, 2L, 3L, "Max", "Mustermann", 1, Optional.empty());
    private ParticipationEligibility participationEligibility1;

    @BeforeEach
    private void setUp() {
        participationEligibilityService = new ParticipationEligibilityService(
                participationEligibilityRepository,
                seasonService,
                clubService);
        participationEligibility1 = new ParticipationEligibility();
        participationEligibility1.setId(1L);
        participationEligibility1.setSeasonId(2L);
        participationEligibility1.setClubId(3L);
        participationEligibility1.setForename("Max");
        participationEligibility1.setSurname("Mustermann");
        participationEligibility1.setPkz(1);
        participationEligibility1.setDwz(null);
    }

    @Test
    void testCreateParticipationEligibilityOk() {
        when(seasonService.seasonExistsById(2L)).thenReturn(Boolean.TRUE);
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

    @Test
    void testCreateParticipationEligibilityClubDoesNotExist() {
        when(seasonService.seasonExistsById(2L)).thenReturn(Boolean.TRUE);
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
        when(clubService.clubExistsById(3L)).thenReturn(Boolean.TRUE);
        BadDataException actualException = assertThrows(BadDataException.class,
                () -> participationEligibilityService.createParticipationEligibility(
                        new ParticipationEligibilityCreationData(2L, 3L, "Max", "Mustermann", 1, Optional.of(-1))
                )
        );
        assertEquals("Die DWZ muss positiv sein!", actualException.getMessage());
    }

    @Test
    void testCreateParticipationEligibilityAlreadyExists() {
        when(seasonService.seasonExistsById(2L)).thenReturn(Boolean.TRUE);
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

}