package de.tonypsilon.bmm.backend.matchday.service;

import de.tonypsilon.bmm.backend.division.service.DivisionService;
import de.tonypsilon.bmm.backend.exception.AlreadyExistsException;
import de.tonypsilon.bmm.backend.exception.BadDataException;
import de.tonypsilon.bmm.backend.exception.NotFoundException;
import de.tonypsilon.bmm.backend.exception.SeasonStageException;
import de.tonypsilon.bmm.backend.matchday.data.CreateMatchdayData;
import de.tonypsilon.bmm.backend.matchday.data.Matchday;
import de.tonypsilon.bmm.backend.matchday.data.MatchdayData;
import de.tonypsilon.bmm.backend.matchday.data.MatchdayRepository;
import de.tonypsilon.bmm.backend.season.service.SeasonService;
import de.tonypsilon.bmm.backend.season.service.SeasonStage;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Set;

@Service
public class MatchdayService {

    private final MatchdayRepository matchdayRepository;
    private final DivisionService divisionService;
    private final SeasonService seasonService;

    public MatchdayService(MatchdayRepository matchdayRepository,
                           DivisionService divisionService,
                           SeasonService seasonService) {
        this.matchdayRepository = matchdayRepository;
        this.divisionService = divisionService;
        this.seasonService = seasonService;
    }

    @Transactional
    public MatchdayData createMatchday(CreateMatchdayData createMatchdayData) {
        if(!divisionService.divisionExistsById(createMatchdayData.divisionId())) {
            throw new NotFoundException("Es gibt keine Staffel mit der ID %d!".formatted(createMatchdayData.divisionId()));
        }
        if(matchdayRepository.existsByDivisionIdAndRound(createMatchdayData.divisionId(), createMatchdayData.round())) {
            throw new AlreadyExistsException("Es gibt für die Staffel mit der ID %d und Runde %d schon einen Spieltag!"
                    .formatted(createMatchdayData.divisionId(), createMatchdayData.round()));
        }
        if(!seasonService.getStageOfSeason(divisionService.getSeasonIdByDivisionId(createMatchdayData.divisionId()))
                .equals(SeasonStage.PREPARATION)) {
            throw new SeasonStageException("Saison ist nicht in der Vorbereitungsphase!");
        }
        verifyRoundNumber(createMatchdayData.divisionId(), createMatchdayData.round());
        verifyMatchdayDate(createMatchdayData.date());

        Matchday matchday = new Matchday();
        matchday.setDivisionId(createMatchdayData.divisionId());
        matchday.setDate(createMatchdayData.date());
        matchday.setRound(createMatchdayData.round());
        matchdayRepository.save(matchday);

        return matchdayToMatchdayData(
                matchdayRepository.getByDivisionIdAndRound(
                        createMatchdayData.divisionId(), createMatchdayData.round()
                )
        );
    }

    public List<MatchdayData> getMatchdaysOfDivisionOrderedByRound(Long divisionId) {
        return matchdayRepository.findByDivisionIdOrderByRoundAsc(divisionId)
                .stream()
                .map(this::matchdayToMatchdayData)
                .toList();
    }

    @Transactional
    public MatchdayData updateMatchday(MatchdayData matchdayData) {
        Matchday matchdayToBeUpdated = matchdayRepository.findById(matchdayData.id())
                .orElseThrow(() -> new NotFoundException("Es gibt keinen Spieltag mit der ID %d!"
                        .formatted(matchdayData.id())));
        if(!matchdayToBeUpdated.getDivisionId().equals(matchdayData.divisionId())) {
            throw new BadDataException("Die Staffel eines Spieltags kann sich nicht ändern!");
        }
        if(!matchdayToBeUpdated.getRound().equals(matchdayData.round())) {
            throw new BadDataException("Die Runde eines Spieltags kann sich nicht ändern!");
        }
        if(!Set.of(SeasonStage.PREPARATION, SeasonStage.RUNNING).contains(
                seasonService.getStageOfSeason(divisionService.getSeasonIdByDivisionId(
                        matchdayToBeUpdated.getDivisionId()
                )))) {
            throw new SeasonStageException("In dieser Saisonphase können Spieltage nicht angepasst werden!");
        }
        verifyMatchdayDate(matchdayData.date());
        matchdayToBeUpdated.setDate(matchdayData.date());
        matchdayRepository.save(matchdayToBeUpdated);
        return matchdayToMatchdayData(
                matchdayRepository.getByDivisionIdAndRound(
                        matchdayData.divisionId(), matchdayData.round()
                )
        );
    }

    @Transactional
    public void deleteMatchday(Long matchdayId) {
        Matchday matchdayToDelete = matchdayRepository.findById(matchdayId).orElseThrow(
                () -> new NotFoundException("Es gibt keinen Spieltag mit der ID %d!".formatted(matchdayId))
        );
        matchdayRepository.delete(matchdayToDelete);
    }

    private void verifyMatchdayDate(String date) {
        if(date == null || date.isBlank()) {
            throw new BadDataException("Das Datum darf nicht leer sein!");
        }
        if (!date.matches("[\\w\\-\\.]+")) {
            throw new BadDataException("Das Spieltagsdatum enthält ungültige Zeichen!");
        }
    }

    private void verifyRoundNumber(Long divisionId, Integer round) {
        List<Integer> currentRounds = matchdayRepository.findByDivisionIdOrderByRoundAsc(divisionId)
                .stream()
                .map(Matchday::getRound)
                .toList();
        // TODO check if rounds are valid sequence.
        if(!round.equals(currentRounds.size()+1)) {
            throw new BadDataException("Für die Staffel mit ID %d sollte als nächstes Runde %d erstellt werden, nicht %s!"
                    .formatted(divisionId, currentRounds.size()+1, round));
        }
    }

    private MatchdayData matchdayToMatchdayData(Matchday matchday) {
        return new MatchdayData(matchday.getId(),
                matchday.getDivisionId(),
                matchday.getDate(),
                matchday.getRound());
    }
}
