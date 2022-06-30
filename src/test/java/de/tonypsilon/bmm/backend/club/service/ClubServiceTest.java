package de.tonypsilon.bmm.backend.club.service;

import de.tonypsilon.bmm.backend.division.data.DivisionRepository;
import de.tonypsilon.bmm.backend.division.service.DivisionService;
import de.tonypsilon.bmm.backend.season.service.SeasonService;
import org.junit.jupiter.api.BeforeEach;

import static org.mockito.Mockito.mock;

class ClubServiceTest {

    private final DivisionRepository divisionRepository = mock(DivisionRepository.class);
    private final SeasonService seasonService = mock(SeasonService.class);
    private DivisionService divisionService;

    @BeforeEach
    private void setUp() {
        this.divisionService = new DivisionService(divisionRepository, seasonService);
    }

}