package de.tonypsilon.bmm.backend;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.web.client.TestRestTemplate;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.test.annotation.Commit;
import org.springframework.test.annotation.Rollback;
import org.springframework.test.context.jdbc.Sql;
import org.springframework.test.context.transaction.AfterTransaction;
import org.springframework.test.context.transaction.BeforeTransaction;
import org.springframework.test.jdbc.JdbcTestUtils;
import org.springframework.transaction.annotation.Transactional;

import javax.sql.DataSource;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Comment snippets and all the annotated methods are copied from the spring reference
 * documentation as a blueprint.
 */
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@Transactional
@Commit
public class BmmApplicationIntegrationTest {

    final JdbcTemplate jdbcTemplate;

    @Autowired
    BmmApplicationIntegrationTest(DataSource dataSource) {
        this.jdbcTemplate = new JdbcTemplate(dataSource);
    }

    @Autowired
    private TestRestTemplate testRestTemplate;

    @BeforeTransaction
    void verifyInitialDatabaseState() {
        // logic to verify the initial state before a transaction is started
    }

    @BeforeEach
    void setUpTestDataWithinTransaction() {
        // set up test data within the transaction
    }

    @Test
    @Sql({"classpath:test-user-data.sql"})
    @Rollback // overrides the class-level @Commit setting
    void modifyDatabaseWithinTransaction() {
        // logic which uses the test data and modifies database state
        assertEquals(1, JdbcTestUtils.countRowsInTable(jdbcTemplate, "users"));
    }

    @AfterEach
    void tearDownWithinTransaction() {
        // run "tear down" logic within the transaction
    }

    @AfterTransaction
    void verifyFinalDatabaseState() {
        // logic to verify the final state after transaction has rolled back
    }
}
