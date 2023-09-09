package de.tonypsilon.bmm.backend.organization.facade;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import de.tonypsilon.bmm.backend.datatypes.IdAndLabel;
import de.tonypsilon.bmm.backend.organization.data.OrganizationData;
import de.tonypsilon.bmm.backend.organization.service.OrganizationAdminService;
import de.tonypsilon.bmm.backend.organization.service.OrganizationService;
import de.tonypsilon.bmm.backend.season.data.SeasonData;
import de.tonypsilon.bmm.backend.season.service.SeasonService;
import de.tonypsilon.bmm.backend.season.service.SeasonStage;
import de.tonypsilon.bmm.backend.security.rnr.Roles;
import de.tonypsilon.bmm.backend.security.rnr.service.AuthorizationService;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.web.servlet.MockMvc;

import java.util.List;
import java.util.Set;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@WebMvcTest(OrganizationController.class)
class OrganizationControllerTest {

    @Autowired
    private MockMvc mockMvc;

    @MockBean
    private OrganizationService organizationService;

    @MockBean
    private OrganizationAdminService organizationAdminService;

    @MockBean
    private SeasonService seasonService;

    @MockBean
    private AuthorizationService authorizationService;

    private final ObjectMapper objectMapper = new ObjectMapper();

    private static final String username = "username";

    @Test
    @WithMockUser(username = username, authorities = {Roles.CLUB_ADMIN})
    void testGetOrganizationsInRegistrationStageForUser() throws Exception {
        OrganizationData organizationWrongSeasonStage =
                new OrganizationData(1L, 2L, "wrong", Set.of(4L));
        OrganizationData organizationRegistrationStage =
                new OrganizationData(2L, 3L, "registration", Set.of(4L));
        when(organizationAdminService.getOrganizationsOfUser(username))
                .thenReturn(Set.of(organizationRegistrationStage, organizationWrongSeasonStage));
        when(seasonService.getStageOfSeason(2L)).thenReturn(SeasonStage.PREPARATION);
        when(seasonService.getStageOfSeason(3L)).thenReturn(SeasonStage.REGISTRATION);
        when(seasonService.getSeasonById(3L)).thenReturn(new SeasonData(3L, "the season", SeasonStage.REGISTRATION));
        var actualResponse = mockMvc
                .perform(get("/organizations/registration"))
                .andExpect(status().isOk())
                .andReturn().getResponse();
        var actual = objectMapper.readValue(actualResponse.getContentAsString(),
                new TypeReference<List<IdAndLabel>>() { });
        assertThat(actual)
                .hasSize(1)
                .containsExactly(new IdAndLabel(2L, "registration - the season"));
    }

}