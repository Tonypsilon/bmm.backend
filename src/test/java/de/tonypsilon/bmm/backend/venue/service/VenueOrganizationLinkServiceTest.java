package de.tonypsilon.bmm.backend.venue.service;

import de.tonypsilon.bmm.backend.venue.data.VenueOrganizationLinkRepository;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.*;
import static org.junit.jupiter.api.Assertions.assertThrows;

class VenueOrganizationLinkServiceTest {

    private final VenueOrganizationLinkRepository venueOrganizationLinkRepository =
            mock(VenueOrganizationLinkRepository.class);
    private final VenueService venueService = mock(VenueService.class);

}