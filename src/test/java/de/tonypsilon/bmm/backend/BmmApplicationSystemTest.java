package de.tonypsilon.bmm.backend;

import com.fasterxml.jackson.databind.ObjectMapper;
import de.tonypsilon.bmm.backend.season.facade.SeasonApiData;
import de.tonypsilon.bmm.backend.season.facade.SeasonName;
import de.tonypsilon.bmm.backend.season.facade.SeasonNamesResponse;
import de.tonypsilon.bmm.backend.season.service.SeasonStage;
import de.tonypsilon.bmm.backend.security.Roles;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

import static org.springframework.security.test.web.servlet.request.SecurityMockMvcRequestPostProcessors.csrf;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;

@SpringBootTest
@AutoConfigureMockMvc
@Transactional
class BmmApplicationSystemTest {

    private final ObjectMapper objectMapper = new ObjectMapper();

    @Autowired
    private MockMvc mockMvc;

    /**
     * Smoke test for basic functionality of the app.
     * Checks a typical happy path.
     * Exceptional cases are tested on unit test level.
     *
     * @throws Exception
     */
    @Test
    @WithMockUser(authorities = Roles.ADMIN)
    void bmmSmokeTest() throws Exception {
        // Step 1: Create a season
        this.mockMvc.perform(post("/seasons")
                        .with(csrf())
                        .param("seasonName", "testSeason"))
                .andExpect(status().isOk())
                .andExpect(content().json(objectMapper.writeValueAsString(new SeasonApiData("testSeason", SeasonStage.PREPARATION))));

        // Step 2: Get the season
        this.mockMvc.perform(get("/seasons/names/non-archived"))
                .andExpect(status().isOk())
                .andExpect(content().json(objectMapper.writeValueAsString(new SeasonNamesResponse(List.of("testSeason")))));

    }

}
