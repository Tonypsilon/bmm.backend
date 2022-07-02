package de.tonypsilon.bmm.backend.division.service;

import de.tonypsilon.bmm.backend.division.data.DivisionRepository;
import org.junit.jupiter.api.BeforeEach;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.mock;

class DivisionServiceTest {

    private final DivisionRepository divisionRepository = mock(DivisionRepository.class);
    private DivisionService divisionService;

    @BeforeEach
    private void setUp() {
        this.divisionService = new DivisionService(divisionRepository);
    }

}