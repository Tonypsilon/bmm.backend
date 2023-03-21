package de.tonypsilon.bmm.backend.season.facade;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import de.tonypsilon.bmm.backend.season.data.SeasonData;
import de.tonypsilon.bmm.backend.season.service.SeasonService;
import de.tonypsilon.bmm.backend.season.service.SeasonStage;
import de.tonypsilon.bmm.backend.season.service.SeasonStageService;
import de.tonypsilon.bmm.backend.security.SecurityConfiguration;
import de.tonypsilon.bmm.backend.security.rnr.service.SeasonAdminService;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.jdbc.AutoConfigureTestDatabase;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Import;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.test.web.servlet.MockMvc;

import java.util.Collection;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@WebMvcTest(SeasonController.class)
@AutoConfigureTestDatabase
@Import(SecurityConfiguration.class)
class SeasonControllerTest {

    @Autowired
    private MockMvc mockMvc;

    @MockBean
    private SeasonService seasonService;

    @MockBean
    private SeasonAdminService seasonAdminService;

    @MockBean
    private SeasonStageService seasonStageService;

    private final ObjectMapper objectMapper = new ObjectMapper();

    private final SeasonData seasonData1 = new SeasonData(1L, "season 1", SeasonStage.PREPARATION);
    private final SeasonData seasonData2 = new SeasonData(2L, "season 2", SeasonStage.COMPLETED);

    @Test
    void testGetAllSeasons()  throws Exception {
        when(seasonService.getAllSeasons()).thenReturn(List.of(seasonData1, seasonData2));
        MockHttpServletResponse actualResponse = mockMvc.perform(get("/seasons"))
                .andExpect(status().isOk())
                .andReturn().getResponse();
        Collection<SeasonData> actualSeasons = objectMapper.readValue(
                actualResponse.getContentAsString(),
                new TypeReference<Collection<SeasonData>>(){});
        assertThat(actualSeasons).hasSize(2);
        assertTrue(actualSeasons.containsAll(List.of(seasonData1, seasonData2)));
    }

}