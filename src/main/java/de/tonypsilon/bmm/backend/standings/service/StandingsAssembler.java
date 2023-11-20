package de.tonypsilon.bmm.backend.standings.service;

import de.tonypsilon.bmm.backend.division.service.DivisionService;
import de.tonypsilon.bmm.backend.match.service.MatchService;
import de.tonypsilon.bmm.backend.matchday.service.MatchdayService;
import de.tonypsilon.bmm.backend.standings.data.DivisionMatchResults;
import de.tonypsilon.bmm.backend.standings.data.StandingsData;
import de.tonypsilon.bmm.backend.team.data.TeamData;
import de.tonypsilon.bmm.backend.team.data.TeamDivisionLinkData;
import de.tonypsilon.bmm.backend.team.service.TeamDivisionLinkService;
import de.tonypsilon.bmm.backend.team.service.TeamService;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
public class StandingsAssembler {

    private final TeamDivisionLinkService teamDivisionLinkService;
    private final TeamService teamService;
    private final DivisionService divisionService;
    private final MatchService matchService;
    private final String vsSelf = "X";

    public StandingsAssembler(
            final TeamDivisionLinkService teamDivisionLinkService,
            final TeamService teamService,
            final DivisionService divisionService,
            final MatchService matchService
            ) {
        this.teamDivisionLinkService = teamDivisionLinkService;
        this.teamService = teamService;
        this.divisionService = divisionService;
        this.matchService = matchService;
    }

    public StandingsData assembleStandings(@NonNull Long divisionId) {
        divisionService.verifyDivisionExists(divisionId);
        List<TeamData> teamsOfDivision = teamDivisionLinkService.getByDivisionId(divisionId).stream()
                .map(TeamDivisionLinkData::teamId)
                .map(teamService::getTeamDataById)
                .toList();
        final DivisionMatchResults results = new DivisionMatchResults(
                teamsOfDivision.stream()
                        .map(TeamData::id)
                        .toList());
        matchService.findByDivision(divisionId)
                .forEach(matchData -> {
                    // Since results are stored in Sets, duplicates will be eliminated.
                    results.addMatch(matchData.homeTeamId(), matchData);
                    results.addMatch(matchData.awayTeamId(), matchData);
                });
        return null; // TODO
    }
}
