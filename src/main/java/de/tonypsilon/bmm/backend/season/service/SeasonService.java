package de.tonypsilon.bmm.backend.season.service;

import de.tonypsilon.bmm.backend.exception.AlreadyExistsException;
import de.tonypsilon.bmm.backend.exception.BadPatchDataException;
import de.tonypsilon.bmm.backend.exception.NameBlankException;
import de.tonypsilon.bmm.backend.exception.NotFoundException;
import de.tonypsilon.bmm.backend.season.data.Season;
import de.tonypsilon.bmm.backend.season.data.SeasonData;
import de.tonypsilon.bmm.backend.season.data.SeasonRepository;
import de.tonypsilon.bmm.backend.season.data.SeasonStageChangeData;
import org.springframework.http.RequestEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Collection;
import java.util.Map;
import java.util.Set;

@Service
public class SeasonService {

    private final SeasonRepository seasonRepository;

    // Valid season stage changes:
    //       ------------------------------------------------> ARCHIVED
    //      /               /            /           /
    // REGISTRATION <-> PREPARATION -> RUNNING -> COMPLETED -> ARCHIVED
    private final Map<SeasonStage, Set<SeasonStage>> validSeasonStageChanges = Map.of(
            SeasonStage.REGISTRATION, Set.of(SeasonStage.REGISTRATION, SeasonStage.PREPARATION, SeasonStage.ARCHIVED),
            SeasonStage.PREPARATION, Set.of(SeasonStage.REGISTRATION, SeasonStage.PREPARATION, SeasonStage.RUNNING, SeasonStage.ARCHIVED),
            SeasonStage.RUNNING, Set.of(SeasonStage.RUNNING, SeasonStage.COMPLETED, SeasonStage.ARCHIVED),
            SeasonStage.COMPLETED, Set.of(SeasonStage.COMPLETED, SeasonStage.ARCHIVED),
            SeasonStage.ARCHIVED, Set.of(SeasonStage.ARCHIVED)
    );

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

    @Transactional
    public SeasonData updateSeasonStage(SeasonStageChangeData seasonStageChangeData) {
        Season seasonToBeUpdated = seasonRepository.findByName(seasonStageChangeData.seasonName())
                .orElseThrow(() -> new NotFoundException("Saison mit dem Namen %s existiert nicht!".formatted(seasonStageChangeData.seasonName())));
        if (validSeasonStageChanges.get(seasonToBeUpdated.getStage()).contains(seasonStageChangeData.stage())) {
            seasonToBeUpdated.setStage(seasonStageChangeData.stage());
            seasonRepository.save(seasonToBeUpdated);
            return seasonToSeasonData(seasonRepository.getByName(seasonStageChangeData.seasonName()));
        } else {
            throw new BadPatchDataException("Eine Saison im Status %s kann nicht in Status %s geändert werden!"
                    .formatted(seasonToBeUpdated.getStage(), seasonStageChangeData.stage()));
        }
    }

    private SeasonData seasonToSeasonData(Season season) {
        return new SeasonData(season.getId(), season.getName(), season.getStage());
    }

}
