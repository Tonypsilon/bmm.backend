package de.tonypsilon.bmm.backend;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.web.client.TestRestTemplate;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.test.context.jdbc.Sql;
import org.springframework.test.jdbc.JdbcTestUtils;
import javax.sql.DataSource;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Comment snippets and all the annotated methods are copied from the spring reference
 * documentation as a blueprint.
 */
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
public class BmmApplicationIntegrationTest {

    final JdbcTemplate jdbcTemplate;

    @Autowired
    BmmApplicationIntegrationTest(DataSource dataSource) {
        this.jdbcTemplate = new JdbcTemplate(dataSource);
    }

    @Autowired
    private TestRestTemplate testRestTemplate;

    @Test
    @Sql(scripts = "classpath:test-user-data.sql",
            executionPhase = Sql.ExecutionPhase.BEFORE_TEST_METHOD)
    @Sql(scripts = "classpath:clear-all-tables.sql",
            executionPhase = Sql.ExecutionPhase.AFTER_TEST_METHOD)
    void modifyDatabaseWithinTransaction() {
        // logic which uses the test data and modifies database state
        assertEquals(1, JdbcTestUtils.countRowsInTable(jdbcTemplate, "users"));
    }

}
