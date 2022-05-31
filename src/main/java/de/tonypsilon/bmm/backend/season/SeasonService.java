package de.tonypsilon.bmm.backend.season;

import de.tonypsilon.bmm.backend.exception.AlreadyExistsException;
import de.tonypsilon.bmm.backend.exception.NameBlankException;
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
    public String createSeason(String seasonName) {
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
        return seasonRepository.getByName(seasonName).getName();
    }
}
