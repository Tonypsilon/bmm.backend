package de.tonypsilon.bmm.backend.club.service;

import de.tonypsilon.bmm.backend.club.data.Club;
import de.tonypsilon.bmm.backend.club.data.ClubCreationData;
import de.tonypsilon.bmm.backend.club.data.ClubData;
import de.tonypsilon.bmm.backend.club.data.ClubRepository;
import de.tonypsilon.bmm.backend.exception.*;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.Collection;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

class ClubServiceTest {

    private final ClubRepository clubRepository = mock(ClubRepository.class);
    private ClubService clubService;
    private Club club1, club2, club3;
    private final ClubData club1Data = new ClubData(1L, "club1", 100, Boolean.TRUE);
    private final ClubData club2Data = new ClubData(2L, "club2", 20, Boolean.TRUE);
    private final ClubData club3Data = new ClubData(3L, "club3", 133, Boolean.TRUE);

    @BeforeEach
    private void setUp() {
        this.clubService = new ClubService(clubRepository);
        club1 = new Club();
        club1.setId(1L);
        club1.setName("club1");
        club1.setZps(100);
        club1.setActive(Boolean.TRUE);
        club2 = new Club();
        club2.setId(2L);
        club2.setName("club2");
        club2.setZps(20);
        club2.setActive(Boolean.TRUE);
        club3 = new Club();
        club3.setId(3L);
        club3.setName("club3");
        club3.setZps(133);
        club3.setActive(Boolean.TRUE);
    }

    @Test
    void testGetAllClubs() {
        when(clubRepository.findAll()).thenReturn(List.of(club1, club2, club3));
        Collection<ClubData> actual = clubService.getAllClubs();
        assertEquals(3, actual.size());
        assertTrue(actual.containsAll(List.of(club1Data, club2Data, club3Data)));
    }

    @Test
    void testCreateClubOk() {
        when(clubRepository.existsByName("club2")).thenReturn(Boolean.FALSE);
        when(clubRepository.existsByZps(20)).thenReturn(Boolean.FALSE);
        when(clubRepository.getByName("club2")).thenReturn(club2);
        ClubData actual = clubService.createClub(new ClubCreationData("club2", 20, Boolean.TRUE));
        assertEquals(club2Data, actual);
        verify(clubRepository, times(1)).save(
                argThat(club -> club.getName().equals("club2")
                && club.getZps().equals(20)
                && club.getActive().equals(Boolean.TRUE))
        );

        when(clubRepository.existsByName("club3")).thenReturn(Boolean.FALSE);
        when(clubRepository.existsByZps(133)).thenReturn(Boolean.FALSE);
        when(clubRepository.getByName("club3")).thenReturn(club3);
        actual = clubService.createClub(new ClubCreationData("club3", 133, null));
        assertEquals(club3Data, actual);
        verify(clubRepository, times(1)).save(
                argThat(club -> club.getName().equals("club3")
                        && club.getZps().equals(133)
                        && club.getActive().equals(Boolean.TRUE))
        );
    }

    @Test
    void testCreateClubInvalidName() {
        NameBlankException nameNullException = assertThrows(NameBlankException.class,
                () -> clubService.createClub(new ClubCreationData(null, 1, Boolean.TRUE)));
        assertEquals("Der Name des Vereins darf nicht leer sein!", nameNullException.getMessage());

        NameBlankException nameBlankException = assertThrows(NameBlankException.class,
                () -> clubService.createClub(new ClubCreationData("", 1, null)));
        assertEquals("Der Name des Vereins darf nicht leer sein!", nameBlankException.getMessage());

    }

    @Test
    void testCreateClubNameAlreadyExists() {
        when(clubRepository.existsByName("club2")).thenReturn(Boolean.TRUE);
        AlreadyExistsException actualException = assertThrows(AlreadyExistsException.class,
                () -> clubService.createClub(new ClubCreationData("club2", 2, Boolean.TRUE)));
        assertEquals("Verein mit dem Namen club2 existiert bereits!", actualException.getMessage());
    }

    @Test
    void testCreateClubWithoutZps() {
        when(clubRepository.existsByName("club2")).thenReturn(Boolean.FALSE);
        MissingDataException actualException = assertThrows(MissingDataException.class,
                () -> clubService.createClub(new ClubCreationData("club2", null, Boolean.TRUE)));
        assertEquals("Verein muss Eigenschaft zps besitzen!", actualException.getMessage());
    }

    @Test
    void testCreateClubZpsAlreadyExists() {
        when(clubRepository.existsByName("club2")).thenReturn(Boolean.FALSE);
        when(clubRepository.existsByZps(20)).thenReturn(Boolean.TRUE);
        AlreadyExistsException actualException = assertThrows(AlreadyExistsException.class,
                () -> clubService.createClub(new ClubCreationData("club2", 20, Boolean.TRUE)));
        assertEquals("Verein mit der zps 20 existiert bereits!", actualException.getMessage());
    }

    @Test
    void testPatchClubOk() {
        Club club1Patched = new Club();
        club1Patched.setId(1L);
        club1Patched.setZps(100);
        club1Patched.setName("club1Patched");
        club1Patched.setActive(Boolean.FALSE);
        when(clubRepository.existsById(1L)).thenReturn(Boolean.TRUE);
        when(clubRepository.getById(1L))
                .thenReturn(club1)
                .thenReturn(club1Patched);

        ClubData actual = clubService.patchClub(new ClubData(1L, "club1Patched", 100, Boolean.FALSE));
        verify(clubRepository, times(1)).save(
                argThat(club -> club.getId().equals(1L)
                        && club.getName().equals("club1Patched")
                        && club.getZps().equals(100)
                        && club.getActive().equals(Boolean.FALSE)
                ));
    }

    @Test
    void testPatchClubThatDoesNotExistOrIdNull() {
        when(clubRepository.existsById(-1L)).thenReturn(Boolean.FALSE);
        NotFoundException idNullException = assertThrows(NotFoundException.class,
                () -> clubService.patchClub(new ClubData(null, "idNullClub", 123, null)));
        assertEquals("Verein mit der ID null existiert nicht!", idNullException.getMessage());
        NotFoundException idNotFoundException = assertThrows(NotFoundException.class,
                () -> clubService.patchClub(new ClubData(-1L, "nonExistentClub", 456, Boolean.TRUE)));
        assertEquals("Verein mit der ID -1 existiert nicht!", idNotFoundException.getMessage());
    }

    @Test
    void testPatchClubZpsChange() {
        when(clubRepository.existsById(1L)).thenReturn(Boolean.TRUE);
        when(clubRepository.getById(1L)).thenReturn(club1);
        BadDataException actualException = assertThrows(BadDataException.class,
                () -> clubService.patchClub(new ClubData(1L, "club1", 1000, Boolean.TRUE)));
        assertEquals("Die Eigenschaft zps eines Vereins darf sich nicht ändern!", actualException.getMessage());
    }

    @Test
    void testClubExistsById() {
        when(clubRepository.existsById(1L)).thenReturn(Boolean.TRUE);
        when(clubRepository.existsById(-1L)).thenReturn(Boolean.FALSE);
        assertEquals(Boolean.TRUE, clubService.clubExistsById(1L));
        assertEquals(Boolean.FALSE, clubService.clubExistsById(-1L));
    }

}