package de.tonypsilon.bmm.backend.divisionsetup.service;

import de.tonypsilon.bmm.backend.division.service.DivisionService;
import de.tonypsilon.bmm.backend.exception.BadDataException;
import de.tonypsilon.bmm.backend.exception.SeasonStageException;
import de.tonypsilon.bmm.backend.season.service.SeasonService;
import de.tonypsilon.bmm.backend.season.service.SeasonStage;
import de.tonypsilon.bmm.backend.team.data.TeamDivisionLinkData;
import de.tonypsilon.bmm.backend.team.service.TeamDivisionLinkService;
import de.tonypsilon.bmm.backend.team.service.TeamService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;

import java.util.List;
import java.util.Set;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatExceptionOfType;
import static org.mockito.ArgumentMatchers.argThat;
import static org.mockito.Mockito.*;

class DivisionSetupServiceTest {
    private final SeasonService seasonService = mock(SeasonService.class);
    private final TeamDivisionLinkService teamDivisionLinkService = mock(TeamDivisionLinkService.class);
    private final TeamService teamService = mock(TeamService.class);
    private final DivisionService divisionService = mock(DivisionService.class);

    private DivisionSetupService divisionSetupService;

    @BeforeEach
    void setUp() {
        this.divisionSetupService = new DivisionSetupService(
                seasonService,
                teamDivisionLinkService,
                teamService,
                divisionService);
    }

    @ParameterizedTest
    @EnumSource(value = SeasonStage.class, names = {"PREPARATION"}, mode = EnumSource.Mode.EXCLUDE)
    void testPutTeamDivisionLinksForSeasonWrongSeasonStage(SeasonStage stage) {
        when(seasonService.getStageOfSeason(1L)).thenReturn(stage);
        assertThatExceptionOfType(SeasonStageException.class)
                .isThrownBy(() ->
                        divisionSetupService.putTeamDivisionLinksForSeason(1L, List.of()))
                .withMessage("Saison ist nicht in der Vorbereitungsphase!");
    }

    @Test
    void testPutTeamDivisionLinksForSeasonTeamFromWrongSeason() {
        when(seasonService.getStageOfSeason(1L)).thenReturn(SeasonStage.PREPARATION);
        when(teamService.getSeasonIdByTeamId(2L)).thenReturn(3L);
        var teamDivisionLinks = List.of(new TeamDivisionLinkData(2L, 4L, 1));
        assertThatExceptionOfType(BadDataException.class)
                .isThrownBy(() ->
                        divisionSetupService.putTeamDivisionLinksForSeason(1L, teamDivisionLinks))
                .withMessage("Mindestens ein Team gehört nicht zur richtigen Saison!");
    }

    @Test
    void testPutTeamDivisionLinksForSeasonDivisionFromWrongSeason() {
        when(seasonService.getStageOfSeason(1L)).thenReturn(SeasonStage.PREPARATION);
        when(teamService.getSeasonIdByTeamId(2L)).thenReturn(1L);
        when(divisionService.getSeasonIdByDivisionId(3L)).thenReturn(4L);
        var teamDivisionLinks = List.of(new TeamDivisionLinkData(2L, 3L, 5));
        assertThatExceptionOfType(BadDataException.class)
                .isThrownBy(() ->
                        divisionSetupService.putTeamDivisionLinksForSeason(1L, teamDivisionLinks))
                .withMessage("Mindestens eine Staffel gehört nicht zur richtigen Saison!");
    }

    @Test
    void testPutDivisionLinksForSeasonDuplicateTeams() {
        when(seasonService.getStageOfSeason(1L)).thenReturn(SeasonStage.PREPARATION);
        when(teamService.getSeasonIdByTeamId(11L)).thenReturn(1L);
        when(divisionService.getSeasonIdByDivisionId(21L)).thenReturn(1L);
        var teamDivisionLinks = List.of(
                new TeamDivisionLinkData(11L, 21L, 1),
                new TeamDivisionLinkData(11L, 21L, 2)
        );
        assertThatExceptionOfType(BadDataException.class)
                .isThrownBy(() ->
                        divisionSetupService.putTeamDivisionLinksForSeason(1L, teamDivisionLinks))
                .withMessage("Jede Mannschaft muss genau ein Mal zugewiesen werden!");
    }

    @Test
    void testPutDivisionLinksForSeasonDuplicateDivisionAndNumber() {
        when(seasonService.getStageOfSeason(1L)).thenReturn(SeasonStage.PREPARATION);
        when(teamService.getSeasonIdByTeamId(argThat(Set.of(11L, 12L)::contains))).thenReturn(1L);
        when(divisionService.getSeasonIdByDivisionId(21L)).thenReturn(1L);
        var teamDivisionLinks = List.of(
                new TeamDivisionLinkData(11L, 21L, 1),
                new TeamDivisionLinkData(12L, 21L, 1)
        );
        assertThatExceptionOfType(BadDataException.class)
                .isThrownBy(() ->
                        divisionSetupService.putTeamDivisionLinksForSeason(1L, teamDivisionLinks))
                .withMessage("Pro Staffel darf höchstens eine Mannschaft pro Platz gesetzt werden!");
    }

    @Test
    void testPutDivisionLinksForSeasonSuccess() {
        when(seasonService.getStageOfSeason(1L)).thenReturn(SeasonStage.PREPARATION);
        when(teamService.getSeasonIdByTeamId(argThat(Set.of(11L, 12L, 13L)::contains))).thenReturn(1L);
        when(divisionService.getSeasonIdByDivisionId(argThat(Set.of(21L, 22L)::contains))).thenReturn(1L);
        TeamDivisionLinkData teamDivisionLinkData1 = new TeamDivisionLinkData(11L, 21L, 1);
        TeamDivisionLinkData teamDivisionLinkData2 = new TeamDivisionLinkData(12L, 21L, 2);
        TeamDivisionLinkData teamDivisionLinkData3 = new TeamDivisionLinkData(13L, 22L, 1);
        var teamDivisionLinks = List.of(
                teamDivisionLinkData1,
                teamDivisionLinkData2,
                teamDivisionLinkData3
        );
        when(teamDivisionLinkService.createTeamDivisionLink(teamDivisionLinkData1)).thenReturn(teamDivisionLinkData1);
        when(teamDivisionLinkService.createTeamDivisionLink(teamDivisionLinkData2)).thenReturn(teamDivisionLinkData2);
        when(teamDivisionLinkService.createTeamDivisionLink(teamDivisionLinkData3)).thenReturn(teamDivisionLinkData3);
        var actual = divisionSetupService.putTeamDivisionLinksForSeason(1L, teamDivisionLinks);
        assertThat(actual)
                .isNotNull()
                .isNotEmpty()
                .containsExactlyInAnyOrder(
                        teamDivisionLinkData1,
                        teamDivisionLinkData2,
                        teamDivisionLinkData3);
        verify(teamDivisionLinkService).deleteBySeason(1L);
        verify(teamDivisionLinkService).createTeamDivisionLink(teamDivisionLinkData1);
        verify(teamDivisionLinkService).createTeamDivisionLink(teamDivisionLinkData2);
        verify(teamDivisionLinkService).createTeamDivisionLink(teamDivisionLinkData3);
        verifyNoMoreInteractions(teamDivisionLinkService);
    }
}