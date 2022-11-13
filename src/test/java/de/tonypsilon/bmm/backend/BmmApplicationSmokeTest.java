package de.tonypsilon.bmm.backend;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import de.tonypsilon.bmm.backend.exception.ErrorData;
import de.tonypsilon.bmm.backend.season.data.SeasonCreationData;
import de.tonypsilon.bmm.backend.season.data.SeasonData;
import de.tonypsilon.bmm.backend.season.service.SeasonStage;
import de.tonypsilon.bmm.backend.security.rnr.Roles;
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
class BmmApplicationSmokeTest {

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
    @WithMockUser(authorities = {Roles.ADMIN, Roles.SEASON_ADMIN})
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
        MockHttpServletResponse getSeasonsResponse = this.mockMvc.perform(get("/seasons"))
                .andExpect(status().isOk())
                .andReturn().getResponse();
        Collection<SeasonData> getSeasonsResult = objectMapper.readValue(getSeasonsResponse.getContentAsString(), new TypeReference<Collection<SeasonData>>() {
        });
        assertEquals(1, getSeasonsResult.size());
        SeasonData actualSeason = getSeasonsResult.iterator().next();
        assertEquals("test", actualSeason.name());
        assertEquals(SeasonStage.REGISTRATION, actualSeason.stage());

        // Step 3: Test an error case: Try to get a season that does not exist.
        // This step implicitly tests the functionality of the BmmExceptionAdvice class.
        MockHttpServletResponse getSeasonThatDoesNotExistResponse = this.mockMvc.perform(get("/seasons/non-existent"))
                .andExpect(status().isBadRequest())
                .andReturn().getResponse();
        ErrorData getSeasonThatDoesNotExistErrorData = objectMapper.readValue(getSeasonThatDoesNotExistResponse.getContentAsString(), ErrorData.class);
        assertEquals("Saison mit dem Namen non-existent existiert nicht!",
                getSeasonThatDoesNotExistErrorData.message());


    }

}
