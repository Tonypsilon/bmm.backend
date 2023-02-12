package de.tonypsilon.bmm.backend.team.service;

import de.tonypsilon.bmm.backend.division.service.DivisionService;
import de.tonypsilon.bmm.backend.exception.BadDataException;
import de.tonypsilon.bmm.backend.exception.SeasonStageException;
import de.tonypsilon.bmm.backend.season.service.SeasonService;
import de.tonypsilon.bmm.backend.season.service.SeasonStage;
import de.tonypsilon.bmm.backend.team.data.TeamDivisionLink;
import de.tonypsilon.bmm.backend.team.data.TeamDivisionLinkData;
import de.tonypsilon.bmm.backend.team.data.TeamDivisionLinkRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;
import org.junit.jupiter.params.provider.ValueSource;

import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class TeamDivisionLinkServiceTest {

    private final TeamDivisionLinkRepository teamDivisionLinkRepository =
            mock(TeamDivisionLinkRepository.class);
    private final TeamService teamService = mock(TeamService.class);
    private final DivisionService divisionService = mock(DivisionService.class);
    private final SeasonService seasonService = mock(SeasonService.class);
    private TeamDivisionLinkService teamDivisionLinkService;
    private TeamDivisionLink teamDivisionLink;

    @BeforeEach
    void setUp() {
        this.teamDivisionLinkService = new TeamDivisionLinkService(
                teamDivisionLinkRepository,
                teamService,
                divisionService,
                seasonService);
        teamDivisionLink = new TeamDivisionLink();
        teamDivisionLink.setTeamId(10L);
        teamDivisionLink.setDivisionId(1L);
        teamDivisionLink.setNumber(2);
    }

    @ParameterizedTest
    @EnumSource(value = SeasonStage.class, mode = EnumSource.Mode.EXCLUDE, names = {"PREPARATION"})
    void testCreateTeamDivisionLinkWrongSeasonStage(SeasonStage seasonStage) {
        TeamDivisionLinkData creationData = new TeamDivisionLinkData(10L, 1L, 2);
        when(divisionService.getSeasonIdByDivisionId(1L)).thenReturn(2L);
        when(seasonService.getStageOfSeason(2L)).thenReturn(seasonStage);

        SeasonStageException actualException = assertThrows(SeasonStageException.class,
                () -> teamDivisionLinkService.createTeamDivisionLink(creationData));
        assertThat(actualException).hasMessage("Saison ist nicht in der Vorbereitungsphase!");
    }

    @Test
    void testCreateTeamDivisionLinkNumberAlreadyTaken() {
        TeamDivisionLinkData creationData = new TeamDivisionLinkData(10L, 1L, 2);
        when(divisionService.getSeasonIdByDivisionId(1L)).thenReturn(2L);
        when(seasonService.getStageOfSeason(2L)).thenReturn(SeasonStage.PREPARATION);
        when(teamDivisionLinkRepository.existsByDivisionIdAndNumber(1L, 2)).thenReturn(Boolean.TRUE);

        BadDataException actualException = assertThrows(BadDataException.class,
                () -> teamDivisionLinkService.createTeamDivisionLink(creationData));

        assertThat(actualException)
                .hasMessage("Es wurde für die Staffel 1 bereits eine Mannschaft auf Platz 2 gesetzt!");
    }

    @Test
    void testCreateTeamDivisionLinkTeamAlreadyAssigned() {
        TeamDivisionLinkData creationData = new TeamDivisionLinkData(10L, 1L, 2);
        when(divisionService.getSeasonIdByDivisionId(1L)).thenReturn(2L);
        when(seasonService.getStageOfSeason(2L)).thenReturn(SeasonStage.PREPARATION);
        when(teamDivisionLinkRepository.existsByDivisionIdAndNumber(1L, 2)).thenReturn(Boolean.FALSE);
        when(teamDivisionLinkRepository.existsByTeamId(10L)).thenReturn(Boolean.TRUE);

        BadDataException actualException = assertThrows(BadDataException.class,
                () -> teamDivisionLinkService.createTeamDivisionLink(creationData));

        assertThat(actualException).hasMessage("Die Mannschaft 10 ist bereits einer Staffel zugeordnet!");
    }

    @ParameterizedTest
    @ValueSource( ints = {-1, 0, 11})
    void testCreateTeamDivisionLinkInvalidNumber(int number) {
        TeamDivisionLinkData creationData = new TeamDivisionLinkData(10L, 1L, number);
        when(divisionService.getSeasonIdByDivisionId(1L)).thenReturn(2L);
        when(seasonService.getStageOfSeason(2L)).thenReturn(SeasonStage.PREPARATION);
        when(teamDivisionLinkRepository.existsByDivisionIdAndNumber(1L, number)).thenReturn(Boolean.FALSE);
        when(teamDivisionLinkRepository.existsByTeamId(10L)).thenReturn(Boolean.FALSE);

        BadDataException actualException = assertThrows(BadDataException.class,
                () -> teamDivisionLinkService.createTeamDivisionLink(creationData));

        assertThat(actualException).hasMessage("Die Setznummer muss eine Zahl zwischen 1 und 10 sein!");
    }

    @Test
    void testCreateTeamDivisionLinkSeasonMismatch() {
        TeamDivisionLinkData creationData = new TeamDivisionLinkData(10L, 1L, 2);
        when(divisionService.getSeasonIdByDivisionId(1L)).thenReturn(2L);
        when(seasonService.getStageOfSeason(2L)).thenReturn(SeasonStage.PREPARATION);
        when(teamDivisionLinkRepository.existsByDivisionIdAndNumber(1L, 2)).thenReturn(Boolean.FALSE);
        when(teamDivisionLinkRepository.existsByTeamId(10L)).thenReturn(Boolean.FALSE);
        when(teamService.getSeasonIdByTeamId(10L)).thenReturn(12L);

        BadDataException actualException = assertThrows(BadDataException.class,
                () -> teamDivisionLinkService.createTeamDivisionLink(creationData));

        assertThat(actualException)
                .hasMessage("Mannschaft 10 und Staffel 1 gehören nicht zur gleichen Saison!");
    }

    @Test
    void testCreateTeamDivisionLinkOk() {
        TeamDivisionLinkData creationData = new TeamDivisionLinkData(10L, 1L, 2);
        when(divisionService.getSeasonIdByDivisionId(1L)).thenReturn(2L);
        when(seasonService.getStageOfSeason(2L)).thenReturn(SeasonStage.PREPARATION);
        when(teamDivisionLinkRepository.existsByDivisionIdAndNumber(1L, 2)).thenReturn(Boolean.FALSE);
        when(teamDivisionLinkRepository.existsByTeamId(10L)).thenReturn(Boolean.FALSE);
        when(teamService.getSeasonIdByTeamId(10L)).thenReturn(2L);

        when(teamDivisionLinkRepository.findByTeamIdAndDivisionId(10L, 1L))
                .thenReturn(Optional.of(teamDivisionLink));

        TeamDivisionLinkData actual =
                teamDivisionLinkService.createTeamDivisionLink(creationData);

        assertThat(actual.teamId()).isEqualTo(10L);
        assertThat(actual.divisionId()).isEqualTo(1L);
        assertThat(actual.number()).isEqualTo(2);
    }

}