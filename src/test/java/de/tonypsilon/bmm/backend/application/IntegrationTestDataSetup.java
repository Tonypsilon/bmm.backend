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
    @Disabled(value = "Only for test data insertion for manual system tests.")
    @Sql(scripts = {"classpath:clear-all-tables.sql",
            "classpath:integrationtest-begin-registration-phase-data.sql"},
            executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    void beginOfRegistrationPhase() {

    }

    @Test
    @Disabled(value = "Only for test data insertion for manual system tests.")
    @Sql(scripts = {"classpath:clear-all-tables.sql",
            "classpath:integrationtest-end-registration-phase-data.sql"},
            executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    void endOfRegistrationPhase() {

    }

    @Test
    @Disabled(value = "Only for test data insertion for manual system tests.")
    @Sql(scripts = {"classpath:clear-all-tables.sql",
            "classpath:integrationtest-preparation-with-dates-and-divisions.sql"},
            executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    void preparationPhaseWithDatesAndDivisions() {

    }

    @Test
    @Disabled(value = "Only for test data insertion for manual system tests.")
    @Sql(scripts = {"classpath:clear-all-tables.sql",
            "classpath:integrationtest-end-preparation-phase.sql"},
            executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    void endPreparationPhase() {

    }

}
