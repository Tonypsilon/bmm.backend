package de.tonypsilon.bmm.backend.match.service;

import de.tonypsilon.bmm.backend.datatypes.IdAndLabel;
import de.tonypsilon.bmm.backend.division.service.DivisionService;
import de.tonypsilon.bmm.backend.exception.*;
import de.tonypsilon.bmm.backend.match.data.*;
import de.tonypsilon.bmm.backend.matchday.data.MatchdayData;
import de.tonypsilon.bmm.backend.matchday.service.MatchdayService;
import de.tonypsilon.bmm.backend.referee.data.RefereeData;
import de.tonypsilon.bmm.backend.referee.service.RefereeService;
import de.tonypsilon.bmm.backend.season.service.SeasonService;
import de.tonypsilon.bmm.backend.season.service.SeasonStage;
import de.tonypsilon.bmm.backend.team.data.TeamData;
import de.tonypsilon.bmm.backend.team.service.TeamDivisionLinkService;
import de.tonypsilon.bmm.backend.team.service.TeamService;
import de.tonypsilon.bmm.backend.validation.service.ValidationService;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;
import java.util.stream.Collectors;


@Service
public class MatchService {

    private final MatchRepository matchRepository;
    private final MatchdayService matchdayService;
    private final TeamService teamService;
    private final RefereeService refereeService;
    private final DivisionService divisionService;
    private final TeamDivisionLinkService teamDivisionLinkService;
    private final ValidationService validationService;
    private final SeasonService seasonService;

    public MatchService(final MatchRepository matchRepository,
                        final MatchdayService matchdayService,
                        final TeamService teamService,
                        final RefereeService refereeService,
                        final DivisionService divisionService,
                        final TeamDivisionLinkService teamDivisionLinkService,
                        final ValidationService validationService,
                        final SeasonService seasonService) {
        this.matchRepository = matchRepository;
        this.matchdayService = matchdayService;
        this.teamService = teamService;
        this.refereeService = refereeService;
        this.divisionService = divisionService;
        this.teamDivisionLinkService = teamDivisionLinkService;
        this.validationService = validationService;
        this.seasonService = seasonService;
    }

    @Transactional
    @NonNull
    public MatchData createMatch(MatchCreationData matchCreationData) {
        MatchdayData matchdayData = matchdayService.getMatchdayDataById(matchCreationData.matchdayId());
        TeamData homeTeamData = teamService.getTeamDataById(matchCreationData.homeTeamId());
        TeamData awayTeamData = teamService.getTeamDataById(matchCreationData.awayTeamId());

        Optional<Long> divisionIdHomeTeam =
                Optional.ofNullable(teamDivisionLinkService.getDivisionIdOfTeam(homeTeamData.id()));
        Optional<Long> divisionIdAwayTeam =
                Optional.ofNullable(teamDivisionLinkService.getDivisionIdOfTeam(awayTeamData.id()));

        if (divisionIdHomeTeam.isEmpty()
        || divisionIdAwayTeam.isEmpty()
        || !divisionIdHomeTeam.get().equals(matchdayData.divisionId())
        || !divisionIdAwayTeam.get().equals(matchdayData.divisionId())) {
            throw new BadDataException("Mindestens eine der beiden Mannschaften gehört nicht zur richtigen Staffel!");
        }

        if(Boolean.TRUE.equals(
                matchRepository.existsByMatchdayIdAndHomeTeamIdOrAwayTeamId(
                        matchCreationData.matchdayId(), matchCreationData.homeTeamId()))) {
            throw new AlreadyExistsException("Die Heimmannschaft hat an diesem Spieltag schon einen Wettkampf!");
        }
        if(Boolean.TRUE.equals(
                matchRepository.existsByMatchdayIdAndHomeTeamIdOrAwayTeamId(
                        matchCreationData.matchdayId(), matchCreationData.awayTeamId()))) {
            throw new AlreadyExistsException("Die Gastmannschaft hat an diesem Spieltag schon einen Wettkampf!");
        }
        Optional.ofNullable(matchCreationData.date()).ifPresent(validationService::validateDateString);
        Long seasonId = divisionService.getSeasonIdByDivisionId(matchdayData.divisionId());
        if (SeasonStage.PREPARATION != matchdayService.getSeasonStageOfMatchday(matchCreationData.matchdayId())) {
            throw new SeasonStageException("Die Saison ist nicht in der Vorbereitungsphase!");
        }
        Optional.ofNullable(matchCreationData.refereeId()).ifPresent(refereeId -> verifyRefereeId(refereeId, seasonId));
        Match match = new Match();
        match.setMatchdayId(matchCreationData.matchdayId());
        match.setHomeTeamId(matchCreationData.homeTeamId());
        match.setAwayTeamId(matchCreationData.awayTeamId());
        Optional.ofNullable(matchCreationData.date()).ifPresent(match::setDate);
        Optional.ofNullable(matchCreationData.refereeId()).ifPresent(match::setRefereeId);
        match.setState(MatchState.OPEN);

        matchRepository.save(match);

        return matchToMatchData(matchRepository.getByMatchdayIdAndHomeTeamIdAndAwayTeamId(
                matchCreationData.matchdayId(), matchCreationData.homeTeamId(), matchCreationData.awayTeamId()));
    }

    @NonNull
    public MatchData getMatchDataById(Long matchId) {
        return matchToMatchData(getById(matchId));
    }

    private Match getById(Long matchId) {
        return matchRepository.findById(matchId)
                .orElseThrow(() -> new NotFoundException("Es gibt keinen Mannschaftskampf mit der ID %d!"
                        .formatted(matchId)));
    }

    @NonNull
    MatchData changeMatchState(@NonNull Long matchId, @NonNull MatchState matchState) {
        Match match = getById(Objects.requireNonNull(matchId));
        match.setState(Objects.requireNonNull(matchState));
        matchRepository.save(match);
        return getMatchDataById(matchId);
    }

    @NonNull
    public List<MatchData> findByMatchdayId(@NonNull Long matchdayId) {
        return matchRepository.findByMatchdayId(matchdayId).stream()
                .sorted(Comparator.comparingLong(Match::getId)) // Should be according to pairing table at some point,
                // for now it is just to have a stable order.
                .map(this::matchToMatchData)
                .toList();
    }

    @NonNull
    MatchData assignReferee(@NonNull Long matchId, @NonNull RefereeData refereeData) {
        Match match = getById(matchId);
        if(!matchdayService.getSeasonIdForMatchday(match.getMatchdayId()).equals(refereeData.seasonId())) {
            throw new BadDataException("Der Schiedsrichter gehört nicht zur passenden Saison!");
        }
        match.setRefereeId(Objects.requireNonNull(refereeData.id()));
        matchRepository.save(match);
        return getMatchDataById(matchId);
    }

    @NonNull
    private MatchData matchToMatchData(@NonNull Match match) {
        return new MatchData(match.getId(),
                match.getMatchdayId(),
                match.getHomeTeamId(),
                match.getAwayTeamId(),
                match.getDate(),
                match.getOverruledHomeBoardHalfPoints(),
                match.getOverruledAwayBoardHalfPoints(),
                match.getRefereeId(),
                match.getVenueId(),
                match.getState());
    }

    private void verifyRefereeId(@NonNull Long refereeId, @NonNull Long seasonId) {
        RefereeData refereeData = refereeService.findById(refereeId)
                .orElseThrow(() -> new NotFoundException("Es gibt keinen Schiedsrichter mit der ID %d!"
                        .formatted(refereeId)));
        if(!refereeData.seasonId().equals(seasonId)) {
            throw new BadDataException("Der Schiedsrichter mit der ID %d passt nicht zur Saison mit der ID %d!"
                    .formatted(refereeId, seasonId));
        }
    }

    public Set<MatchData> getAllOpenMatchesOfRunningSeasons() {
        return matchRepository.findAll().stream()
                .filter(match ->
                        MatchState.OPEN == match.getState())
                .filter(match ->
                        SeasonStage.RUNNING == matchdayService.getSeasonStageOfMatchday(match.getMatchdayId()))
                .map(this::matchToMatchData)
                .collect(Collectors.toSet());
    }

    @NonNull
    public Set<MatchData> findByDivision(@NonNull Long divisionId) {
        return matchdayService.getMatchdaysOfDivisionOrderedByRound(divisionId)
                .stream()
                .map(MatchdayData::id)
                .map(this::findByMatchdayId)
                .flatMap(List::stream)
                .collect(Collectors.toSet());
    }

    @NonNull
    public List<IdAndLabel> getMatchInfosOfOpenMatchesForTeam(@NonNull Long teamId) {
        return getAllOpenMatchesOfRunningSeasons().stream()
                .filter(matchData -> matchData.homeTeamId().equals(teamId) || matchData.awayTeamId().equals(teamId))
                .sorted(Comparator.comparingInt(matchData ->
                        matchdayService.getMatchdayDataById(matchData.matchdayId()).round()))
                .map(matchData -> new IdAndLabel(matchData.id(), getLabelOfMatch(matchData)))
                .toList();
    }

    @NonNull
    private String getLabelOfMatch(@NonNull MatchData matchData) {
        MatchdayData matchdayData = matchdayService.getMatchdayDataById(matchData.matchdayId());
        String seasonName =  seasonService.getSeasonById(
                divisionService.getSeasonIdByDivisionId(matchdayData.divisionId()))
                .name();
        return seasonName + " - R" + matchdayData.round() + " - "
                + teamService.getNameOfTeam(matchData.homeTeamId())
                + " : "
                + teamService.getNameOfTeam(matchData.awayTeamId());
    }
}
