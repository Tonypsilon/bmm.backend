package de.tonypsilon.bmm.backend;

import com.fasterxml.jackson.databind.ObjectMapper;
import de.tonypsilon.bmm.backend.season.data.SeasonCreationData;
import de.tonypsilon.bmm.backend.season.service.SeasonStage;
import de.tonypsilon.bmm.backend.security.Roles;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.MediaType;
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
                        .contentType(MediaType.APPLICATION_JSON_VALUE)
                        .with(csrf())
                        .content(objectMapper.writeValueAsString(new SeasonCreationData("test"))))
                .andExpect(status().isCreated());

        // Step 2: Get the season
        this.mockMvc.perform(get("/seasons/non-archived"))
                .andExpect(status().isOk());

    }

}
