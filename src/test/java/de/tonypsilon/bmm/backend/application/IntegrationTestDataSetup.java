package de.tonypsilon.bmm.backend.application;

import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.web.server.LocalServerPort;
import org.springframework.test.context.jdbc.Sql;

@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
public class IntegrationTestDataSetup {

    @LocalServerPort
    private Integer port;

    @Test
    @Disabled
    @Sql(scripts = {"classpath:clear-all-tables.sql",
            "classpath:integrationtest-begin-preparation-phase-data.sql"},
            executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    void beginOfRegistrationPhase() {

    }

    @Test
    @Disabled
    @Sql(scripts = {"classpath:clear-all-tables.sql",
            "classpath:integrationtest-end-preparation-phase-data.sql"},
            executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    void endOfRegistrationPhase() {

    }

}
