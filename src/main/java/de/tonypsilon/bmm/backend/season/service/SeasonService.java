package de.tonypsilon.bmm.backend.season.service;

import de.tonypsilon.bmm.backend.exception.AlreadyExistsException;
import de.tonypsilon.bmm.backend.exception.NameBlankException;
import de.tonypsilon.bmm.backend.season.data.Season;
import de.tonypsilon.bmm.backend.season.data.SeasonData;
import de.tonypsilon.bmm.backend.season.data.SeasonRepository;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Service
public class SeasonService {

    private final SeasonRepository seasonRepository;

    public SeasonService(SeasonRepository seasonRepository) {
        this.seasonRepository = seasonRepository;
    }

    public List<String> getAllNonArchivedSeasonNames() {
        return seasonRepository.findAll().stream()
                .filter(s -> !s.getStage().equals(SeasonStage.ARCHIVED))
                .map(Season::getName)
                .sorted(String::compareTo)
                .toList();
    }

    @Transactional
    public SeasonData createSeason(String seasonName) {
        if(seasonName == null || seasonName.isBlank()) {
            throw new NameBlankException("Der Name der Saison darf nicht leer sein!");
        }
        if(Boolean.TRUE.equals(seasonRepository.existsByName(seasonName))) {
            throw new AlreadyExistsException("Saison mit diesem Namen existiert bereits!");
        }
        Season season = new Season();
        season.setName(seasonName);
        season.setStage(SeasonStage.PREPARATION);
        seasonRepository.save(season);
        return seasonToSeasonData(seasonRepository.getByName(seasonName));
    }

    public SeasonData getSeasonByName(String seasonName) {
        return seasonToSeasonData(seasonRepository.getByName(seasonName));
    }

    private SeasonData seasonToSeasonData(Season season) {
        return new SeasonData(season.getId(), season.getName(), season.getStage());
    }
}
