package de.tonypsilon.bmm.backend.match.service;

import de.tonypsilon.bmm.backend.division.service.DivisionService;
import de.tonypsilon.bmm.backend.exception.AlreadyExistsException;
import de.tonypsilon.bmm.backend.exception.BadDataException;
import de.tonypsilon.bmm.backend.exception.NotFoundException;
import de.tonypsilon.bmm.backend.match.data.CreateMatchData;
import de.tonypsilon.bmm.backend.match.data.Match;
import de.tonypsilon.bmm.backend.match.data.MatchData;
import de.tonypsilon.bmm.backend.match.data.MatchRepository;
import de.tonypsilon.bmm.backend.matchday.data.MatchdayData;
import de.tonypsilon.bmm.backend.matchday.service.MatchdayService;
import de.tonypsilon.bmm.backend.referee.data.RefereeData;
import de.tonypsilon.bmm.backend.referee.service.RefereeService;
import de.tonypsilon.bmm.backend.team.data.TeamData;
import de.tonypsilon.bmm.backend.team.service.TeamDivisionLinkService;
import de.tonypsilon.bmm.backend.team.service.TeamService;
import de.tonypsilon.bmm.backend.validation.service.ValidationService;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Optional;


@Service
public class MatchService {

    private final MatchRepository matchRepository;
    private final MatchdayService matchdayService;
    private final TeamService teamService;
    private final RefereeService refereeService;
    private final DivisionService divisionService;
    private final TeamDivisionLinkService teamDivisionLinkService;
    private final ValidationService validationService;

    public MatchService(final MatchRepository matchRepository,
                        final MatchdayService matchdayService,
                        final TeamService teamService,
                        final RefereeService refereeService,
                        final DivisionService divisionService,
                        final TeamDivisionLinkService teamDivisionLinkService,
                        final ValidationService validationService) {
        this.matchRepository = matchRepository;
        this.matchdayService = matchdayService;
        this.teamService = teamService;
        this.refereeService = refereeService;
        this.divisionService = divisionService;
        this.teamDivisionLinkService = teamDivisionLinkService;
        this.validationService = validationService;
    }

    @Transactional
    @NonNull
    public MatchData createMatch(CreateMatchData createMatchData) {
        MatchdayData matchdayData = matchdayService.getMatchdayDataById(createMatchData.matchdayId());
        TeamData homeTeamData = teamService.getTeamDataById(createMatchData.homeTeamId());
        TeamData awayTeamData = teamService.getTeamDataById(createMatchData.awayTeamId());

        Optional<Long> divisionIdHomeTeam =
                Optional.ofNullable(teamDivisionLinkService.getDivisionIdOfTeam(homeTeamData.id()));
        Optional<Long> divisionIdAwayTeam =
                Optional.ofNullable(teamDivisionLinkService.getDivisionIdOfTeam(awayTeamData.id()));

        if (divisionIdHomeTeam.isEmpty()
        || divisionIdAwayTeam.isEmpty()
        || divisionIdHomeTeam.get().equals(matchdayData.divisionId())
        || divisionIdAwayTeam.get().equals(matchdayData.divisionId())) {
            throw new BadDataException("Mindestens eine der beiden Mannschaften gehört nicht zur richtigen Staffel!");
        }

        if(Boolean.TRUE.equals(
                matchRepository.existsByMatchdayIdAndHomeTeamIdOrAwayTeamId(
                        createMatchData.matchdayId(), createMatchData.homeTeamId()))) {
            throw new AlreadyExistsException("Die Heimmannschaft hat an diesem Spieltag schon einen Wettkampf!");
        }
        if(Boolean.TRUE.equals(
                matchRepository.existsByMatchdayIdAndHomeTeamIdOrAwayTeamId(
                        createMatchData.matchdayId(), createMatchData.awayTeamId()))) {
            throw new AlreadyExistsException("Die Gastmannschaft hat an diesem Spieltag schon einen Wettkampf!");
        }
        createMatchData.date().ifPresent(validationService::validateDateString);
        Long seasonId = divisionService.getSeasonIdByDivisionId(matchdayData.divisionId());
        createMatchData.refereeId().ifPresent(refereeId -> verifyRefereeId(refereeId, seasonId));
        Match match = new Match();
        match.setMatchdayId(createMatchData.matchdayId());
        match.setHomeTeamId(createMatchData.homeTeamId());
        match.setAwayTeamId(createMatchData.awayTeamId());
        createMatchData.date().ifPresent(match::setDate);
        createMatchData.refereeId().ifPresent(match::setRefereeId);
        match.setHomeTeamPoints(0);
        match.setAwayTeamPoints(0);
        match.setEditable(Boolean.TRUE);

        matchRepository.save(match);

        return matchToMatchData(matchRepository.getByMatchdayIdAndHomeTeamIdAndAwayTeamId(
                createMatchData.matchdayId(), createMatchData.homeTeamId(), createMatchData.awayTeamId()));
    }

    @NonNull
    public MatchData getMatchById(Long matchId) {
        return matchRepository.findById(matchId).map(this::matchToMatchData)
                .orElseThrow(() -> new NotFoundException("Es gibt keinen Mannschaftskampf mit der ID %d!"
                        .formatted(matchId)));
    }

    @NonNull
    private MatchData matchToMatchData(@NonNull Match match) {
        return new MatchData(match.getId(),
                match.getMatchdayId(),
                match.getHomeTeamId(),
                match.getAwayTeamId(),
                match.getDate(),
                match.getHomeTeamPoints(),
                match.getAwayTeamPoints(),
                match.getOverruledHomeBoardHalfPoints(),
                match.getOverruledAwayBoardHalfPoints(),
                match.getRefereeId(),
                match.getEditable());
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

}
