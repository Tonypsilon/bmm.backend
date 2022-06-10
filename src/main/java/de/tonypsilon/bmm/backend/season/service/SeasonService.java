package de.tonypsilon.bmm.backend.season.service;

import de.tonypsilon.bmm.backend.exception.AlreadyExistsException;
import de.tonypsilon.bmm.backend.exception.NameBlankException;
import de.tonypsilon.bmm.backend.exception.NotFoundException;
import de.tonypsilon.bmm.backend.season.data.Season;
import de.tonypsilon.bmm.backend.season.data.SeasonData;
import de.tonypsilon.bmm.backend.season.data.SeasonRepository;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Collection;

@Service
public class SeasonService {

    private final SeasonRepository seasonRepository;

    public SeasonService(SeasonRepository seasonRepository) {
        this.seasonRepository = seasonRepository;
    }

    @Transactional
    public SeasonData createSeason(String seasonName) {
        if(seasonName == null || seasonName.isBlank()) {
            throw new NameBlankException("Der Name der Saison darf nicht leer sein!");
        }
        if(Boolean.TRUE.equals(seasonRepository.existsByName(seasonName))) {
            throw new AlreadyExistsException("Saison mit dem Namen %s existiert bereits!".formatted(seasonName));
        }
        Season season = new Season();
        season.setName(seasonName);
        season.setStage(SeasonStage.REGISTRATION);
        seasonRepository.save(season);
        return seasonToSeasonData(seasonRepository.getByName(seasonName));
    }

    public SeasonData getNonArchivedSeasonByName(String seasonName) {
        return seasonToSeasonData(seasonRepository.findByName(seasonName).filter(season -> !season.getStage().equals(SeasonStage.ARCHIVED))
                .orElseThrow(() -> new NotFoundException("Nichtarchivierte Saison mit dem Namen %s existiert nicht!".formatted(seasonName))));
    }

    public SeasonData getArchivedSeasonByName(String seasonName) {
        return seasonToSeasonData(seasonRepository.findByName(seasonName).filter(season -> season.getStage().equals(SeasonStage.ARCHIVED))
                .orElseThrow(() -> new NotFoundException("Archivierte Saison mit dem Namen %s existiert nicht!".formatted(seasonName))));
    }

    public Collection<SeasonData> getAllNonArchivedSeasons() {
        return seasonRepository.findAll()
                .stream()
                .filter(season -> !season.getStage().equals(SeasonStage.ARCHIVED))
                .map(this::seasonToSeasonData)
                .toList();
    }

    public Collection<SeasonData> getAllArchivedSeasons() {
        return seasonRepository.findAll()
                .stream()
                .filter(season -> season.getStage().equals(SeasonStage.ARCHIVED))
                .map(this::seasonToSeasonData)
                .toList();
    }

    private SeasonData seasonToSeasonData(Season season) {
        return new SeasonData(season.getId(), season.getName(), season.getStage());
    }
}
