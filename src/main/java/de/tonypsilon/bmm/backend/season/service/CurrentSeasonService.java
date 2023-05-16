package de.tonypsilon.bmm.backend.season.service;

import de.tonypsilon.bmm.backend.season.data.CurrentSeason;
import de.tonypsilon.bmm.backend.season.data.CurrentSeasonRepository;
import de.tonypsilon.bmm.backend.season.data.SeasonData;
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
        return seasonService.getSeasonById(currentSeasonRepository.findAll().get(0).getSeasonId());
    }

}
