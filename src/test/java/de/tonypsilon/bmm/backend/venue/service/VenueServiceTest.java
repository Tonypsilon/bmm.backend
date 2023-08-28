package de.tonypsilon.bmm.backend.venue.service;

import de.tonypsilon.bmm.backend.club.service.ClubService;
import de.tonypsilon.bmm.backend.exception.AlreadyExistsException;
import de.tonypsilon.bmm.backend.exception.BadDataException;
import de.tonypsilon.bmm.backend.exception.NotFoundException;
import de.tonypsilon.bmm.backend.organization.service.OrganizationService;
import de.tonypsilon.bmm.backend.validation.service.ValidationService;
import de.tonypsilon.bmm.backend.venue.data.*;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullAndEmptySource;

import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.*;
import static org.junit.jupiter.api.Assertions.assertThrows;

class VenueServiceTest {

    private final VenueRepository venueRepository = mock(VenueRepository.class);
    private final ClubService clubService = mock(ClubService.class);
    private final OrganizationService organizationService = mock(OrganizationService.class);
    private VenueService venueService;
    private Venue venue;
    private static final String stringOfLength128 = "1abcdefghijklmnopqrstuvwx2abcdefghijklmnopqrstuvwx" +
            "3abcdefghijklmnopqrstuvwx4abcdefghijklmnopqrstuvwx5abcdefghijklmnopqrstuvwx123";

    @BeforeEach
    void setUp() {
        venueService = new VenueService(venueRepository, clubService, new ValidationService(), organizationService);
        venue = new Venue();
        venue.setId(2L);
        venue.setClubId(1L);
        venue.setAddress("address");
        venue.setHints("hints");
    }

    @Test
    void testCreateVenueClubNull() {
        VenueCreationData creationData = new VenueCreationData(null, "address", null);
        BadDataException actualException = assertThrows(BadDataException.class,
                () -> venueService.createVenue(creationData));
    }

    @ParameterizedTest
    @NullAndEmptySource
    void testCreateVenueAddressMissing(String address) {
        VenueCreationData creationData = new VenueCreationData(1L, address, null);
        BadDataException actualException = assertThrows(BadDataException.class,
                () -> venueService.createVenue(creationData));
        assertThat(actualException).hasMessage("Es muss eine Adresse gegeben sein!");
    }

    @Test
    void testCreateVenueTooLongAddress() {
        VenueCreationData creationData = new VenueCreationData(1L, stringOfLength128+"x", null);
        BadDataException actualException = assertThrows(BadDataException.class,
                () -> venueService.createVenue(creationData));
        assertThat(actualException).hasMessage("Die Zeichenkette ist zu lang!");
    }

    @Test
    void testCreateVenueTooLongHints() {
        VenueCreationData creationData = new VenueCreationData(1L, "address",
                stringOfLength128 + stringOfLength128 + "x");
        BadDataException actualException = assertThrows(BadDataException.class,
                () -> venueService.createVenue(creationData));
        assertThat(actualException).hasMessage("Die Zeichenkette ist zu lang!");
    }

    @Test
    void testCreateVenueAlreadyExists() {
        VenueCreationData creationData = new VenueCreationData(1L, "address", null);
        when(venueRepository.existsByClubIdAndAddress(1L, "address")).thenReturn(Boolean.TRUE);
        AlreadyExistsException actualException = assertThrows(AlreadyExistsException.class,
                () -> venueService.createVenue(creationData));
        assertThat(actualException)
                .hasMessage("Es gibt bereits einen Spielort unter der Adresse address für den Verein 1!");
    }

    @Test
    void testCreateVenueOk() {
        VenueCreationData creationData = new VenueCreationData(1L, "address", "hints");
        when(venueRepository.existsByClubIdAndAddress(1L, "address")).thenReturn(Boolean.FALSE);
        when(venueRepository.getByClubIdAndAddress(1L, "address")).thenReturn(venue);
        VenueData actual = venueService.createVenue(creationData);
        verify(venueRepository).save(argThat(venue -> venue.getClubId().equals(1L)
                && venue.getAddress().equals("address")
                && venue.getHints().isPresent()
                && venue.getHints().get().equals("hints")));
        assertThat(actual.id()).isEqualTo(2L);
        assertThat(actual.clubId()).isEqualTo(1L);
        assertThat(actual.address()).isEqualTo("address");
        assertThat(actual.hints()).isNotNull().isEqualTo("hints");
    }

    @Test
    void testVerifyVenueExistsOk() {
        when(venueRepository.findById(1L)).thenReturn(Optional.of(venue));
        venueService.verifyVenueExistsById(1L);
        verify(venueRepository).findById(1L);
    }

    @Test
    void testVerifyVenueExistsFail() {
        when(venueRepository.findById(1L)).thenReturn(Optional.empty());
        NotFoundException actualException = assertThrows(NotFoundException.class,
                () -> venueService.verifyVenueExistsById(1L));
        assertThat(actualException).hasMessage("Es gibt keinen Spielort mit ID 1!");
    }
}