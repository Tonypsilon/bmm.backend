package de.tonypsilon.bmm.backend.dwz.service;

import de.tonypsilon.bmm.backend.club.data.ClubData;
import de.tonypsilon.bmm.backend.club.service.ClubService;
import de.tonypsilon.bmm.backend.division.data.DivisionData;
import de.tonypsilon.bmm.backend.division.service.DivisionService;
import de.tonypsilon.bmm.backend.dwz.data.DwzAssemblingContext;
import de.tonypsilon.bmm.backend.game.data.GameData;
import de.tonypsilon.bmm.backend.game.service.GameService;
import de.tonypsilon.bmm.backend.match.data.MatchData;
import de.tonypsilon.bmm.backend.match.service.MatchService;
import de.tonypsilon.bmm.backend.matchday.data.MatchdayData;
import de.tonypsilon.bmm.backend.matchday.service.MatchdayService;
import de.tonypsilon.bmm.backend.participant.data.ParticipantData;
import de.tonypsilon.bmm.backend.participant.service.ParticipantService;
import de.tonypsilon.bmm.backend.participationeligibility.data.ParticipationEligibilityData;
import de.tonypsilon.bmm.backend.participationeligibility.service.ParticipationEligibilityService;
import de.tonypsilon.bmm.backend.season.data.SeasonData;
import de.tonypsilon.bmm.backend.season.service.PlayingDateService;
import de.tonypsilon.bmm.backend.season.service.SeasonService;
import de.tonypsilon.bmm.backend.team.service.TeamDivisionLinkService;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Service;

import java.util.*;
import java.util.function.Function;
import java.util.stream.IntStream;

import static java.util.Comparator.comparing;
import static java.util.stream.Collectors.toMap;
import static java.util.stream.Collectors.toSet;

@Service
public class DwzAssembler {

    private final DivisionService divisionService;
    private final ParticipantService participantService;
    private final ParticipationEligibilityService participationEligibilityService;
    private final TeamDivisionLinkService teamDivisionLinkService;
    private final GameService gameService;
    private final MatchdayService matchdayService;
    private final MatchService matchService;
    private final ClubService clubService;
    private final PlayingDateService playingDateService;
    private final SeasonService seasonService;

    public DwzAssembler(final DivisionService divisionService,
                        final ParticipantService participantService,
                        final ParticipationEligibilityService participationEligibilityService,
                        final TeamDivisionLinkService teamDivisionLinkService,
                        final GameService gameService,
                        final MatchdayService matchdayService,
                        final MatchService matchService,
                        final ClubService clubService,
                        final PlayingDateService playingDateService,
                        final SeasonService seasonService) {
        this.divisionService = divisionService;
        this.participantService = participantService;
        this.participationEligibilityService = participationEligibilityService;
        this.teamDivisionLinkService = teamDivisionLinkService;
        this.gameService = gameService;
        this.matchdayService = matchdayService;
        this.matchService = matchService;
        this.clubService = clubService;
        this.playingDateService = playingDateService;
        this.seasonService = seasonService;
    }

    @NonNull
    public String assembleDwz(@NonNull String seasonName, @NonNull String divisionName) {
        DivisionData divisionData = divisionService.getDivisionDataBySeasonNameAndDivisionName(seasonName, divisionName);
        SeasonData seasonData = seasonService.getSeasonByName(seasonName);
        Set<ParticipantData> participants = new HashSet<>(teamDivisionLinkService.getByDivisionId(divisionData.id()).stream()
                .map(teamDivisionLinkData -> participantService.getParticipantsEligibleForTeam(teamDivisionLinkData.teamId()))
                .flatMap(Collection::stream)
                .filter(participant -> gameService.findByParticipantId(participant.id()).stream()
                        .anyMatch(gameData -> {
                            MatchData matchData = matchService.getMatchDataById(gameData.matchId());
                            MatchdayData matchdayData = matchdayService.getMatchdayDataById(matchData.matchdayId());
                            return matchdayData.divisionId().equals(divisionData.id());
                        }))
                .collect(toMap(ParticipantData::id, Function.identity(), (p1, p2) -> p1))
                .values());
        Map<ParticipantData, ParticipationEligibilityData> participationEligibilities = participants.stream()
                .collect(toMap(Function.identity(), this::getParticipationEligibilityData));
        List<ParticipantData> participantsSorted = participants.stream()
                .sorted(comparing(this::participantSurname).thenComparing(this::participantForename))
                .toList();
        Map<Long,Integer> participantsOrdinal = IntStream.range(0, participantsSorted.size())
                .boxed()
                .collect(toMap(i -> participantsSorted.get(i).id(), i -> i+1));
        Map<Long, Map<Integer, GameData>> gamesByRoundByParticipants = participants.stream()
                .collect(toMap(ParticipantData::id, participantData -> {
                    List<GameData> games = gameService.findByParticipantId(participantData.id());
                    return games.stream()
                            .filter(game -> gameMatchesDivision(game, divisionData.id()))
                            .collect(toMap(this::roundOfGame, Function.identity()));
                }));
        var context = new DwzAssemblingContext(divisionData.numberOfTeams() -1,
                gamesByRoundByParticipants,
                participantsOrdinal);
        StringBuilder result = new StringBuilder();
        result.append(buildHeader(divisionName, seasonName)).append('\n');
        int i = 1;
        for (ParticipantData participant : participantsSorted) {
            result.append(' ').append(formatI(i++)).append('.').append(" ".repeat(5));
            String forename = participantForename(participant);
            String surname = participantSurname(participant);
            result.append(surname).append(',').append(forename)
                    .append(" ".repeat(32 - forename.length() - surname.length()));
            ClubData club = clubService.getClubById(participationEligibilities.get(participant).clubId());
            result.append(club.name()).append(" ".repeat(33 - club.name().length()))
                    .append(" ".repeat(46))
                    .append(club.zps()).append(" ".repeat(7))
                    .append(context.getGamesByParticipant(participant.id())).append('\n');
        }
        result.append(buildFooter(divisionName, seasonName,
                playingDateService.getBySeasonIdAndNumber(seasonData.id(), 1).date(),
                playingDateService.getBySeasonIdAndNumber(seasonData.id(), divisionData.numberOfTeams() -1).date()));
        return result.toString();
    }

    private String formatI(int i) {
        if (i < 0 ) {
            throw new IllegalArgumentException("i must be positive.");
        }
        if (i < 10) {
            return "  " + i;
        }
        if (i < 100) {
            return " " + i;
        }
        if (i < 1000) {
            return String.valueOf(i);
        }
        throw new IllegalArgumentException("i must be less than 1000.");
    }

    private Integer roundOfGame(GameData gameData) {
        return matchdayService.getMatchdayDataById(
                matchService.getMatchDataById(gameData.matchId()).matchdayId())
                .round();
    }

    private boolean gameMatchesDivision(GameData gameData, Long divisionId) {
        MatchData matchData = matchService.getMatchDataById(gameData.matchId());
        MatchdayData matchdayData = matchdayService.getMatchdayDataById(matchData.matchdayId());
        return matchdayData.divisionId().equals(divisionId);
    }

    private ParticipationEligibilityData getParticipationEligibilityData(ParticipantData participantData) {
        return participationEligibilityService.getParticipationEligibilityById(participantData.participationEligibilityId());
    }

    private String participantSurname(ParticipantData participantData) {
        return participationEligibilityService.getParticipationEligibilityById(participantData.participationEligibilityId()).surname();
    }

    private String participantForename(ParticipantData participantData) {
        return participationEligibilityService.getParticipationEligibilityById(participantData.participationEligibilityId()).forename();
    }

    private String buildHeader(final String divisionName,
                               final String seasonName) {
        return "Berliner Schachverband \n"
                + divisionName + " " + seasonName + "\n"
                + " ttt. rrr nnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn "
                + "vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv "
                + "lll ffffffffff pppppppppp gggggggg eeee dddd  zzzzz mmmm ";
    }

    private String buildFooter(final String divisionName,
                               final String seasonName,
                               final String firstRoundDate,
                               final String lastRoundDate) {
        return "### \n"
                + "Name:       " + divisionName + " " + seasonName + "\n"
                + "Ort:        Berlin\n"
                + "FIDE-Land:  GER\n"
                + "Datum(S):   %s           Datum(E):   %s".formatted(firstRoundDate, lastRoundDate);
    }
}
