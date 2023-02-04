package de.tonypsilon.bmm.backend.season.service;

import de.tonypsilon.bmm.backend.exception.AlreadyExistsException;
import de.tonypsilon.bmm.backend.exception.BadDataException;
import de.tonypsilon.bmm.backend.exception.NameBlankException;
import de.tonypsilon.bmm.backend.exception.NotFoundException;
import de.tonypsilon.bmm.backend.season.data.*;
import org.springframework.lang.NonNull;
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
    @NonNull
    public SeasonData createSeason(@NonNull SeasonCreationData seasonCreationData) {
        if(seasonCreationData.name() == null || seasonCreationData.name().isBlank()) {
            throw new NameBlankException("Der Name der Saison darf nicht leer sein!");
        }
        if(Boolean.TRUE.equals(seasonRepository.existsByName(seasonCreationData.name()))) {
            throw new AlreadyExistsException("Saison mit dem Namen %s existiert bereits!".formatted(seasonCreationData.name()));
        }
        Season season = new Season();
        season.setName(seasonCreationData.name());
        season.setStage(SeasonStage.REGISTRATION);
        seasonRepository.save(season);
        return seasonToSeasonData(seasonRepository.getByName(seasonCreationData.name()));
    }

    @NonNull
    public SeasonData getSeasonByName(@NonNull String seasonName) {
        return seasonToSeasonData(seasonRepository.findByName(seasonName)
                .orElseThrow(() -> new NotFoundException("Saison mit dem Namen %s existiert nicht!".formatted(seasonName))));
    }

    public SeasonData getSeasonById(Long seasonId) {
        return seasonToSeasonData(seasonRepository.findById(seasonId)
                .orElseThrow(() -> new NotFoundException("Saison mit der ID %d existiert nicht!".formatted(seasonId))));
    }

    @NonNull
    public Collection<SeasonData> getAllSeasons() {
        return seasonRepository.findAll()
                .stream()
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
            throw new BadDataException("Eine Saison im Status %s kann nicht in Status %s geändert werden!"
                    .formatted(seasonToBeUpdated.getStage(), seasonStageChangeData.stage()));
        }
    }

    @NonNull
    public Boolean seasonExistsById(@NonNull Long seasonId) {
        return seasonRepository.existsById(seasonId);
    }

    @NonNull
    public SeasonStage getStageOfSeason(Long seasonId) {
        return seasonRepository.findById(seasonId).orElseThrow(
                () -> new NotFoundException("Saison mit ID %d existiert nicht!".formatted(seasonId))
        ).getStage();
    }

    @NonNull
    private SeasonData seasonToSeasonData(@NonNull Season season) {
        return new SeasonData(season.getId(), season.getName(), season.getStage());
    }

}
