package de.tonypsilon.bmm.backend.season.service;

import de.tonypsilon.bmm.backend.exception.BmmException;
import de.tonypsilon.bmm.backend.season.data.*;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
public class CurrentSeasonService {

    private final CurrentSeasonRepository currentSeasonRepository;
    private final SeasonService seasonService;

    public CurrentSeasonService(final CurrentSeasonRepository currentSeasonRepository,
                                final SeasonService seasonService) {
        this.currentSeasonRepository = currentSeasonRepository;
        this.seasonService = seasonService;
    }

    @NonNull
    @Transactional
    public SeasonData setCurrentSeason(@NonNull Long seasonId) {
        seasonService.getSeasonById(seasonId);
        currentSeasonRepository.deleteAll();
        CurrentSeason currentSeason = new CurrentSeason();
        currentSeason.setSeasonId(seasonId);
        currentSeasonRepository.save(currentSeason);
        return getCurrentSeason();
    }

    @NonNull
    public SeasonData getCurrentSeason() {
        return currentSeasonRepository.findAll().stream().findAny()
                .map(CurrentSeason::getSeasonId)
                .map(seasonService::getSeasonById)
                .orElseThrow(() -> new BmmException("No current season set!"));
    }

}
