package de.tonypsilon.bmm.backend.match.service;

import de.tonypsilon.bmm.backend.exception.AlreadyExistsException;
import de.tonypsilon.bmm.backend.exception.BadDataException;
import de.tonypsilon.bmm.backend.exception.NotFoundException;
import de.tonypsilon.bmm.backend.match.data.CreateMatchData;
import de.tonypsilon.bmm.backend.match.data.Match;
import de.tonypsilon.bmm.backend.match.data.MatchData;
import de.tonypsilon.bmm.backend.match.data.MatchRepository;
import de.tonypsilon.bmm.backend.matchday.service.MatchdayService;
import de.tonypsilon.bmm.backend.team.service.TeamService;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
public class MatchService {

    private final MatchRepository matchRepository;
    private final MatchdayService matchdayService;
    private final TeamService teamService;

    public MatchService(final MatchRepository matchRepository,
                        final MatchdayService matchdayService,
                        final TeamService teamService) {
        this.matchRepository = matchRepository;
        this.matchdayService = matchdayService;
        this.teamService = teamService;
    }

    @Transactional
    @NonNull
    public MatchData createMatch(CreateMatchData createMatchData) {
        if(!matchRepository.existsById(createMatchData.matchdayId())) {
            throw new NotFoundException("Es gibt keinen Spieltag mit der ID %d!"
                    .formatted(createMatchData.matchdayId()));
        }
        if(!teamService.existsById(createMatchData.homeTeamId())) {
            throw new NotFoundException("Die Heimmannschaft mit ID %d existiert nicht!"
                    .formatted(createMatchData.homeTeamId()));
        }
        if(!teamService.existsById(createMatchData.awayTeamId())) {
            throw new NotFoundException("Die Gastmannschaft mit ID %d existiert nicht!"
                    .formatted(createMatchData.awayTeamId()));
        }
        if(matchRepository.existsByHomeTeamIdOrAwayTeamId(createMatchData.homeTeamId())) {
            throw new AlreadyExistsException("Die Heimmannschaft hat an diesem Spieltag schon einen Wettkampf!");
        }
        if(matchRepository.existsByHomeTeamIdOrAwayTeamId(createMatchData.awayTeamId())) {
            throw new AlreadyExistsException("Die Gastmannschaft hat an diesem Spieltag schon einen Wettkampf!");
        }
        createMatchData.date().ifPresent(this::verifyMatchdayDate);
        // TODO Referee service check, once the referee service is implemented.
        Match match = new Match();
        match.setMatchdayId(createMatchData.matchdayId());
        match.setHomeTeamId(createMatchData.homeTeamId());
        match.setAwayTeamId(createMatchData.awayTeamId());
        createMatchData.date().ifPresent(match::setDate);
        createMatchData.refereeId().ifPresent(match::setRefereeId);
        match.setHomeTeamPoints(0);
        match.setAwayTeamPoints(0);

        matchRepository.save(match);

        return matchToMatchData(matchRepository.getByMatchdayIdAndHomeTeamIdAndAwayTeamId(
                createMatchData.matchdayId(), createMatchData.homeTeamId(), createMatchData.awayTeamId()));
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
                match.getRefereeId());
    }

    private void verifyMatchdayDate(String date) {
        if(date.isBlank()) {
            throw new BadDataException("Das Wettkampfsdatum darf nicht leer sein!");
        }
        if (!date.matches("[\\w\\-\\.]+")) {
            throw new BadDataException("Das Wettkampfsdatum enthält ungültige Zeichen!");
        }
    }

}
