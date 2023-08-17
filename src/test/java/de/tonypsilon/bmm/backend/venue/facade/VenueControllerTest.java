package de.tonypsilon.bmm.backend.venue.facade;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import de.tonypsilon.bmm.backend.security.rnr.Roles;
import de.tonypsilon.bmm.backend.security.rnr.service.AuthorizationService;
import de.tonypsilon.bmm.backend.venue.data.VenueData;
import de.tonypsilon.bmm.backend.venue.service.VenueService;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.MediaType;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.web.servlet.MockMvc;

import java.util.List;
import java.util.Set;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.argThat;
import static org.mockito.Mockito.*;
import static org.springframework.security.test.web.servlet.request.SecurityMockMvcRequestPostProcessors.csrf;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@WebMvcTest(VenueController.class)
class VenueControllerTest {

    @Autowired
    private MockMvc mockMvc;

    @MockBean
    private VenueService venueService;

    @MockBean
    private AuthorizationService authorizationService;

    private final ObjectMapper objectMapper = new ObjectMapper();

    private static final String username = "username";

    @Test
    @WithMockUser(username = username, authorities = {Roles.CLUB_ADMIN})
    void testPutVenuesForClub() throws Exception {
        var putVenuesData = List.of(new VenueData(1L, 2L, "address1", "hints1"),
                new VenueData(null, 2L, "address2", null),
                new VenueData(3L, 4L, "address3", "hints3"));
        when(venueService.updateVenue(any()))
                .thenAnswer(invocationOnMock -> invocationOnMock.getArgument(0, VenueData.class));
        var actualResponse = mockMvc
                .perform(put("/venues/club/2")
                        .with(csrf())
                        .content(objectMapper.writeValueAsString(putVenuesData))
                        .contentType(MediaType.APPLICATION_JSON_VALUE))
                .andExpect(status().isOk())
                .andReturn().getResponse();
        var actual = objectMapper.readValue(
                actualResponse.getContentAsString(),
                new TypeReference<List<VenueData>>() {
                }
        );
        assertThat(actual).hasSize(2);
        verify(venueService).createVenue(argThat(venueCreationData ->
                venueCreationData.clubId().equals(2L)
                        && venueCreationData.address().equals("address2")
                        && venueCreationData.hints() == null));
        verify(venueService).updateVenue(argThat(venueData ->
                venueData.id().equals(1L)
                        && venueData.clubId().equals(2L)
                        && venueData.address().equals("address1")
                        && "hints1".equals(venueData.hints())));
        verifyNoMoreInteractions(venueService);
        verify(authorizationService).verifyUserIsClubAdminOfAnyClub(username, Set.of(2L));
        verifyNoMoreInteractions(authorizationService);
    }

}