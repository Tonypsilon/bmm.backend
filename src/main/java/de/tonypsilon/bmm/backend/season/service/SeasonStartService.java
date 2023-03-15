package de.tonypsilon.bmm.backend.season.service;

import de.tonypsilon.bmm.backend.division.data.DivisionData;
import de.tonypsilon.bmm.backend.division.service.DivisionService;
import de.tonypsilon.bmm.backend.match.data.CreateMatchData;
import de.tonypsilon.bmm.backend.match.service.MatchService;
import de.tonypsilon.bmm.backend.matchday.data.CreateMatchdayData;
import de.tonypsilon.bmm.backend.matchday.data.MatchdayData;
import de.tonypsilon.bmm.backend.matchday.service.MatchdayService;
import de.tonypsilon.bmm.backend.season.data.SeasonData;
import de.tonypsilon.bmm.backend.season.pairings.Pairing;
import de.tonypsilon.bmm.backend.season.pairings.PairingTable;
import de.tonypsilon.bmm.backend.team.data.TeamDivisionLinkData;
import de.tonypsilon.bmm.backend.team.service.TeamDivisionLinkService;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Service;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

@Service
public class SeasonStartService {

    private final MatchdayService matchdayService;
    private final MatchService matchService;
    private final PlayingDateService playingDateService;
    private final DivisionService divisionService;
    private final TeamDivisionLinkService teamDivisionLinkService;

    /* This is hardcoded for now, but might become more flexible in the future. */
    private static final int NUMBER_OF_ROUNDS = 9;

    public SeasonStartService(final MatchdayService matchdayService,
                              final MatchService matchService,
                              final PlayingDateService playingDateService,
                              final DivisionService divisionService,
                              final TeamDivisionLinkService teamDivisionLinkService) {
        this.matchdayService = matchdayService;
        this.matchService = matchService;
        this.playingDateService = playingDateService;
        this.divisionService = divisionService;
        this.teamDivisionLinkService = teamDivisionLinkService;
    }

    /**
     * Validation must be done prior (all teams assigned etc.)
     * For now, only divisions of 10 teams are supported.
     *
     * @param seasonData
     */
    void createMatchesForSeason(@NonNull SeasonData seasonData) {
        divisionService.getAllDivisionsOfSeason(seasonData.id()).forEach(this::createMatchesForDivision);
    }

    private void createMatchesForDivision(@NonNull DivisionData divisionData) {
        PairingTable pairingTable = PairingTable.STANDARD;
        Map<Integer, TeamDivisionLinkData> teamsOfDivisionByNumber =
                teamDivisionLinkService.getByDivisionId(divisionData.id())
                .stream()
                .collect(Collectors.toMap(TeamDivisionLinkData::number,
                        Function.identity()));
        for (int i = 1; i <= NUMBER_OF_ROUNDS; i++) {
            MatchdayData matchdayData = matchdayService.createMatchday(
                    new CreateMatchdayData(divisionData.id(),
                            playingDateService.getBySeasonIdAndNumber(divisionData.seasonId(), i).date(),
                            i));
            List<Pairing> pairingsOfRound = pairingTable.getPairingsOfRound(i);
            for (Pairing pairing : pairingsOfRound) {
                matchService.createMatch(new CreateMatchData(matchdayData.id(),
                        teamsOfDivisionByNumber.get(pairing.home()).teamId(),
                        teamsOfDivisionByNumber.get(pairing.away()).teamId(),
                        Optional.empty(),
                        Optional.empty()));
            }
        }
    }

}
