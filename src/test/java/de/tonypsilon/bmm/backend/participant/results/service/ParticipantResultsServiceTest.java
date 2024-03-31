package de.tonypsilon.bmm.backend.participant.results.service;

import de.tonypsilon.bmm.backend.datatypes.IdAndLabel;
import de.tonypsilon.bmm.backend.game.data.GameData;
import de.tonypsilon.bmm.backend.game.service.GameService;
import de.tonypsilon.bmm.backend.game.service.Result;
import de.tonypsilon.bmm.backend.match.data.MatchData;
import de.tonypsilon.bmm.backend.match.data.MatchState;
import de.tonypsilon.bmm.backend.match.service.MatchService;
import de.tonypsilon.bmm.backend.matchday.data.MatchdayData;
import de.tonypsilon.bmm.backend.matchday.service.MatchdayService;
import de.tonypsilon.bmm.backend.participant.data.ParticipantData;
import de.tonypsilon.bmm.backend.participant.results.data.ParticipantResultData;
import de.tonypsilon.bmm.backend.participant.results.data.ParticipantResultsData;
import de.tonypsilon.bmm.backend.participant.service.ParticipantService;
import de.tonypsilon.bmm.backend.participationeligibility.data.ParticipationEligibilityData;
import de.tonypsilon.bmm.backend.participationeligibility.service.ParticipationEligibilityService;
import de.tonypsilon.bmm.backend.season.data.SeasonData;
import de.tonypsilon.bmm.backend.season.service.SeasonService;
import de.tonypsilon.bmm.backend.season.service.SeasonStage;
import de.tonypsilon.bmm.backend.team.data.TeamData;
import de.tonypsilon.bmm.backend.team.service.TeamService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class ParticipantResultsServiceTest {

    private final GameService gameService = mock(GameService.class);
    private final ParticipantService participantService = mock(ParticipantService.class);
    private final ParticipationEligibilityService participationEligibilityService =
            mock(ParticipationEligibilityService.class);
    private final TeamService teamService = mock(TeamService.class);
    private final MatchService matchService = mock(MatchService.class);
    private final MatchdayService matchdayService = mock(MatchdayService.class);
    private final SeasonService seasonService = mock(SeasonService.class);
    private ParticipantResultsService participantResultsService;

    private final long theParticipantId = 1L;
    private final long theParticipationEligibilityId = 3L;
    private final long opponent1Id = 10L;
    private final long opponent2Id = 11L;
    private final long opponent1EligibilityId = 12L;
    private final long opponent2EligibilityId = 13L;
    private final long game1Id = 100L;
    private final long game2Id = 101L;
    private final long match1Id = 102L;
    private final long match2Id = 103L;
    private final long matchday1Id = 104L;
    private final long matchday2Id = 105L;

    private final ParticipantData theParticipant = new ParticipantData(
            theParticipantId,
            2L,
            theParticipationEligibilityId,
            1);

    private final ParticipantData opponent1 = new ParticipantData(
            opponent1Id,
            20L,
            opponent1EligibilityId,
            1);

    private final ParticipantData opponent2 = new ParticipantData(
            opponent2Id,
            21L,
            opponent2EligibilityId,
            1);

    private final ParticipationEligibilityData theParticipationEligibility = new ParticipationEligibilityData(
            theParticipationEligibilityId,
            7L,
            8L,
            "Max",
            "Mustermann",
            1234,
            1900);

    private final ParticipationEligibilityData opponent1Eligibility = new ParticipationEligibilityData(
            opponent1EligibilityId,
            7L,
            80L,
            "Erika",
            "Musterfrau",
            1235,
            1900);

    private final ParticipationEligibilityData opponent2Eligibility = new ParticipationEligibilityData(
            opponent2EligibilityId,
            7L,
            81L,
            "Erik",
            "Muster",
            1236,
            null);

    private final GameData game1 = new GameData(
            game1Id,
            match1Id,
            1,
            theParticipantId,
            opponent1Id,
            Optional.of(Result.DRAW),
            Optional.empty(),
            Optional.of(Result.DRAW),
            Optional.empty());

    private final GameData game2 = new GameData(
            game2Id,
            match2Id,
            1,
            opponent2Id,
            theParticipantId,
            Optional.of(Result.DRAW),
            Optional.of(Result.LOSS),
            Optional.of(Result.DRAW),
            Optional.of(Result.WIN));

    @BeforeEach
    public void setUp() {
        this.participantResultsService = new ParticipantResultsService(
                gameService,
                participantService,
                participationEligibilityService,
                teamService,
                matchService,
                matchdayService,
                seasonService);
    }
    @Test
    void testGetResultsForParticipantNoGames() {
        when(participantService.getParticipantById(theParticipantId)).thenReturn(theParticipant);
        when(participationEligibilityService.getParticipationEligibilityById(theParticipationEligibilityId))
                .thenReturn(theParticipationEligibility);
        when(participationEligibilityService.getParticipationEligibilityById(opponent1EligibilityId))
                .thenReturn(opponent1Eligibility);
        when(participationEligibilityService.getParticipationEligibilityById(opponent2EligibilityId))
                .thenReturn(opponent2Eligibility);
        when(teamService.getTeamDataById(theParticipant.teamId()))
                .thenReturn(new TeamData(theParticipant.teamId(),1L, 1, 1L, "The Team 1", "captain"));
        when(teamService.getTeamDataById(opponent1.teamId()))
                .thenReturn(new TeamData(opponent1.teamId(),2L, 1, 2L, "Opponent Team 1", "captain"));
        when(teamService.getTeamDataById(opponent2.teamId()))
                .thenReturn(new TeamData(opponent2.teamId(),3L, 1, 3L, "Opponent Team 2", "captain"));
        when(gameService.findByParticipantId(theParticipantId)).thenReturn(List.of(game1,game2));
        when(seasonService.getSeasonById(7L))
                .thenReturn(new SeasonData(7L, "the season", SeasonStage.RUNNING));
        when(matchService.getMatchDataById(match1Id))
                .thenReturn(new MatchData(
                        match1Id,
                        matchday1Id,
                        theParticipant.teamId(),
                        opponent1.teamId(),
                        Optional.empty(),
                        Optional.empty(),
                        Optional.empty(),
                        Optional.empty(),
                        Optional.empty(),
                        MatchState.CLOSED));
        when(matchService.getMatchDataById(match2Id))
                .thenReturn(new MatchData(
                        match2Id,
                        matchday2Id,
                        opponent2.teamId(),
                        theParticipant.teamId(),
                        Optional.empty(),
                        Optional.empty(),
                        Optional.empty(),
                        Optional.empty(),
                        Optional.empty(),
                        MatchState.CLOSED));
        when(matchdayService.getMatchdayDataById(matchday1Id)).thenReturn(
                new MatchdayData(matchday1Id, 1L, "1.1.2000", 1));
        when(matchdayService.getMatchdayDataById(matchday2Id)).thenReturn(
                new MatchdayData(matchday2Id, 1L, "2.2.2000", 2));
        when(participantService.getParticipantById(opponent1Id)).thenReturn(opponent1);
        when(participationEligibilityService.getParticipationEligibilityById(opponent1EligibilityId))
                .thenReturn(opponent1Eligibility);
        when(participantService.getParticipantById(opponent2Id)).thenReturn(opponent2);
        when(participationEligibilityService.getParticipationEligibilityById(opponent2EligibilityId))
                .thenReturn(opponent2Eligibility);

        var actual = participantResultsService.getResultsForParticipant(theParticipantId);
        assertThat(actual).isEqualTo(new ParticipantResultsData(
                "Max Mustermann",
                "1900",
                new IdAndLabel(theParticipant.teamId(), "The Team 1"),
                List.of(
                        new ParticipantResultData(
                                1,
                                "black",
                                new IdAndLabel(20L, "Opponent Team 1"),
                                1,
                                new IdAndLabel(10L, "Erika Musterfrau (1900)"),
                                "½"
                        ),
                        new ParticipantResultData(
                                2,
                                "white",
                                new IdAndLabel(21L, "Opponent Team 2"),
                                1,
                                new IdAndLabel(11L, "Erik Muster"),
                                "1"
                        )
                ),
                "",
                "",
                "the season"
        ));
    }

}