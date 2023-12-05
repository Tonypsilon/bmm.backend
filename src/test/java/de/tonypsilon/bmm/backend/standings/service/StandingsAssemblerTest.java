package de.tonypsilon.bmm.backend.standings.service;

import de.tonypsilon.bmm.backend.division.service.DivisionService;
import de.tonypsilon.bmm.backend.exception.NotFoundException;
import de.tonypsilon.bmm.backend.game.service.GameService;
import de.tonypsilon.bmm.backend.match.service.MatchService;
import de.tonypsilon.bmm.backend.team.service.TeamDivisionLinkService;
import de.tonypsilon.bmm.backend.team.service.TeamService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatExceptionOfType;
import static org.mockito.Mockito.*;

class StandingsAssemblerTest {

    private StandingsAssembler standingsAssembler;
    private final TeamDivisionLinkService teamDivisionLinkService = mock(TeamDivisionLinkService.class);
    private final TeamService teamService =  mock(TeamService.class);
    private final DivisionService divisionService = mock(DivisionService.class);
    private final MatchService matchService = mock(MatchService.class);
    private final GameService gameService = mock(GameService.class);

    @BeforeEach
    public void setUp() {
        this.standingsAssembler = new StandingsAssembler(
                teamDivisionLinkService,
                teamService,
                divisionService,
                matchService,
                gameService
        );
    }

    @Test
    void testAssembleStandingsDivisionDoesNotExist() {
        doThrow(new NotFoundException("Es gibt keine Staffel mit der ID 1!"))
                .when(divisionService).verifyDivisionExists(1L);
        assertThatExceptionOfType(NotFoundException.class)
                .isThrownBy(() -> standingsAssembler.assembleStandings(1L))
                .withMessage("Es gibt keine Staffel mit der ID 1!");
    }
}