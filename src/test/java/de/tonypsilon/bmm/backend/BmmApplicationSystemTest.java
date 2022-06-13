package de.tonypsilon.bmm.backend;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import de.tonypsilon.bmm.backend.season.data.SeasonCreationData;
import de.tonypsilon.bmm.backend.season.data.SeasonData;
import de.tonypsilon.bmm.backend.season.data.SeasonStageChangeData;
import de.tonypsilon.bmm.backend.season.service.SeasonStage;
import de.tonypsilon.bmm.backend.security.Roles;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.MediaType;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.transaction.annotation.Transactional;

import java.util.Collection;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.springframework.security.test.web.servlet.request.SecurityMockMvcRequestPostProcessors.csrf;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
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
        MockHttpServletResponse postSeasonResponse = this.mockMvc.perform(post("/seasons")
                        .contentType(MediaType.APPLICATION_JSON_VALUE)
                        .with(csrf())
                        .content(objectMapper.writeValueAsString(new SeasonCreationData("test"))))
                .andExpect(status().isCreated())
                .andReturn().getResponse();
        SeasonData postSeasonResult = objectMapper.readValue(postSeasonResponse.getContentAsString(), SeasonData.class);
        assertEquals("test", postSeasonResult.name());
        assertEquals(SeasonStage.REGISTRATION, postSeasonResult.stage());

        // Step 2: Get the season
        MockHttpServletResponse getSeasonsResponse = this.mockMvc.perform(get("/seasons/non-archived"))
                .andExpect(status().isOk())
                .andReturn().getResponse();
        Collection<SeasonData> getSeasonsResult = objectMapper.readValue(getSeasonsResponse.getContentAsString(), new TypeReference<Collection<SeasonData>>() {
        });
        assertEquals(1, getSeasonsResult.size());
        SeasonData actualSeason = getSeasonsResult.iterator().next();
        assertEquals("test", actualSeason.name());
        assertEquals(SeasonStage.REGISTRATION, actualSeason.stage());

        // Step 3: Patch the season to archived
        MockHttpServletResponse patchSeasonResponse = this.mockMvc.perform(patch("/seasons/test")
                        .contentType(MediaType.APPLICATION_JSON_VALUE)
                        .with(csrf())
                        .content(objectMapper.writeValueAsString(new SeasonStageChangeData("test", SeasonStage.ARCHIVED))))
                .andExpect(status().isOk())
                .andReturn().getResponse();

        SeasonData patchSeasonResult = objectMapper.readValue(patchSeasonResponse.getContentAsString(), SeasonData.class);
        assertEquals("test", patchSeasonResult.name());
        assertEquals(SeasonStage.ARCHIVED, patchSeasonResult.stage());

        // Step 4: Get the season again, this time as archived
        getSeasonsResponse = this.mockMvc.perform(get("/seasons/archived"))
                .andExpect(status().isOk())
                .andReturn().getResponse();
        getSeasonsResult = objectMapper.readValue(getSeasonsResponse.getContentAsString(), new TypeReference<Collection<SeasonData>>() {
        });
        assertEquals(1, getSeasonsResult.size());
        actualSeason = getSeasonsResult.iterator().next();
        assertEquals("test", actualSeason.name());
        assertEquals(SeasonStage.ARCHIVED, actualSeason.stage());
    }

}
