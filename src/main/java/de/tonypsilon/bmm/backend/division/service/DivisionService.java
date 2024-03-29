package de.tonypsilon.bmm.backend.division.service;

import com.google.common.collect.SortedSetMultimap;
import com.google.common.collect.TreeMultimap;
import de.tonypsilon.bmm.backend.division.data.*;
import de.tonypsilon.bmm.backend.exception.*;
import de.tonypsilon.bmm.backend.season.data.SeasonData;
import de.tonypsilon.bmm.backend.season.service.SeasonService;
import de.tonypsilon.bmm.backend.season.service.SeasonStage;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;

@Service
public class DivisionService {

    private final DivisionRepository divisionRepository;
    private final SeasonService seasonService;

    public DivisionService(final DivisionRepository divisionRepository,
                           final SeasonService seasonService) {
        this.divisionRepository = divisionRepository;
        this.seasonService = seasonService;
    }

    @NonNull
    public SortedSetMultimap<Integer, DivisionData> getAllDivisionsOfSeasonByLevel(@NonNull Long seasonId) {
        Collection<Division> divisions = divisionRepository.findBySeasonId(seasonId);
        SortedSetMultimap<Integer, DivisionData> divisionsOfSeasonByLevel = TreeMultimap.create(
                Integer::compareTo, Comparator.comparing(DivisionData::name));
        for (Division division : divisions) {
            divisionsOfSeasonByLevel.put(division.getLevel(), divisionToDivisionData(division));
        }
        return divisionsOfSeasonByLevel;
    }

    @NonNull
    public List<DivisionData> getAllDivisionsOfSeason(@NonNull Long seasonId) {
        return divisionRepository.findBySeasonId(seasonId).stream()
                .map(this::divisionToDivisionData)
                .sorted(Comparator.comparingInt(DivisionData::level).thenComparing(DivisionData::name))
                .toList();
    }

    @Transactional
    @NonNull
    public DivisionData createDivision(@NonNull DivisionCreationData divisionCreationData) {
        if(divisionCreationData.name() == null || divisionCreationData.name().isBlank()) {
            throw new NameBlankException("Der Name der Staffel darf nicht leer sein!");
        }
        if (divisionCreationData.seasonId() == null) {
            throw new BadDataException("Zur Erstellung einer Staffel muss eine Saison gegeben sein!");
        }
        if(!seasonService.seasonExistsById(divisionCreationData.seasonId())) {
            throw new NotFoundException("Es gibt keine Saison mit der ID %d!"
                    .formatted(divisionCreationData.seasonId()));
        }
        if(seasonService.getStageOfSeason(divisionCreationData.seasonId()) != SeasonStage.PREPARATION) {
            throw new SeasonStageException("Saison ist nicht in der Vorbereitungsphase!");
        }
        if (Boolean.TRUE.equals(divisionRepository.existsBySeasonIdAndName(divisionCreationData.seasonId(), divisionCreationData.name()))) {
            throw new AlreadyExistsException("Staffel mit Namen %s für Saison mit ID %d existiert bereits!"
                    .formatted(divisionCreationData.name(), divisionCreationData.seasonId()));
        }
        if(divisionCreationData.numberOfBoards() == null || divisionCreationData.numberOfBoards() < 1) {
            throw new BadDataException("Die Anzahl der Bretter für eine Staffel muss eine ganze Zahl > 0 sein!");
        }
        if(divisionCreationData.level() == null || divisionCreationData.level() < 1) {
            throw new BadDataException("Das Level der Staffel muss eine ganze Zahl > 0 sein!");
        }
        if(!Set.of(8, 10).contains(divisionCreationData.numberOfTeams())) {
            throw new BadDataException("Eine Staffel muss 8 oder 10 Mannschaften haben!");
        }
        Division division = new Division();
        division.setName(divisionCreationData.name());
        division.setSeasonId(divisionCreationData.seasonId());
        division.setLevel(divisionCreationData.level());
        division.setNumberOfBoards(divisionCreationData.numberOfBoards());
        division.setNumberOfTeams(divisionCreationData.numberOfTeams());
        divisionRepository.save(division);
        return divisionToDivisionData(
                divisionRepository.getBySeasonIdAndName(divisionCreationData.seasonId(), divisionCreationData.name()));
    }

    @NonNull
    public Boolean divisionExistsById(@NonNull Long divisionId) {
        return divisionRepository.existsById(divisionId);
    }

    public void verifyDivisionExists(@NonNull Long divisionId) {
        getById(divisionId);
    }

    @NonNull
    public Long getSeasonIdByDivisionId(@NonNull Long divisionId) {
        return getById(divisionId).getSeasonId();
    }

    @NonNull
    public Integer getNumberOfBoardsByDivisionId(@NonNull Long divisionId) {
        return getById(divisionId).getNumberOfBoards();
    }

    @NonNull
    public DivisionData getDivisionDataBySeasonNameAndDivisionName(
            @NonNull String seasonName, @NonNull String divisionName) {
        SeasonData seasonData = seasonService.getSeasonByName(seasonName);
        return divisionToDivisionData(
                divisionRepository.findBySeasonIdAndName(seasonData.id(), divisionName).orElseThrow(
                        () -> new NotFoundException(
                                "Es gibt keine Staffel mit dem Namen %s in der Saison mit Namen %s!"
                                        .formatted(divisionName, seasonName))
                ));
    }

    @NonNull
    public DivisionData getDivisionDataById(@NonNull Long divisionId) {
        return divisionToDivisionData(getById(divisionId));
    }

    @NonNull private Division getById(@NonNull Long divisionId) {
        return divisionRepository.findById(divisionId)
                .orElseThrow(
                        () -> new NotFoundException("Es gibt keine Staffel mit der ID %d!"
                                .formatted(divisionId))
                );
    }

    @NonNull
    private DivisionData divisionToDivisionData(@NonNull Division division) {
        return new DivisionData(division.getId(),
                division.getName(),
                division.getLevel(),
                division.getNumberOfBoards(),
                division.getSeasonId(),
                division.getNumberOfTeams());
    }

}
