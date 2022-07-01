package de.tonypsilon.bmm.backend.division.service;

import com.google.common.collect.Multimap;
import com.google.common.collect.SortedSetMultimap;
import com.google.common.collect.TreeMultimap;
import de.tonypsilon.bmm.backend.division.data.Division;
import de.tonypsilon.bmm.backend.division.data.DivisionData;
import de.tonypsilon.bmm.backend.division.data.DivisionRepository;
import de.tonypsilon.bmm.backend.season.data.SeasonData;
import de.tonypsilon.bmm.backend.season.service.SeasonService;

import java.util.Collection;
import java.util.Comparator;
import java.util.List;

public class DivisionService {

    private final DivisionRepository divisionRepository;
    private final SeasonService seasonService;

    public DivisionService(DivisionRepository divisionRepository,
                           SeasonService seasonService) {
        this.divisionRepository = divisionRepository;
        this.seasonService = seasonService;
    }

    public Collection<DivisionData> getAllNonArchivedDivisions() {
        List<Long> nonArchivedSeasonsIds = seasonService
                .getAllNonArchivedSeasons()
                .stream()
                .map(SeasonData::id)
                .toList();
        return divisionRepository.findBySeasonIdIn(nonArchivedSeasonsIds)
                .stream()
                .map(this::divisionToDivisionData)
                .toList();
    }

    public Collection<DivisionData> getAllArchivedDivisionsByLevel() {
        List<Long> archivedSeasonsIds = seasonService
                .getAllArchivedSeasons()
                .stream()
                .map(SeasonData::id)
                .toList();
        return divisionRepository.findBySeasonIdIn(archivedSeasonsIds)
                .stream()
                .map(this::divisionToDivisionData)
                .toList();
    }

    public SortedSetMultimap<Integer, DivisionData> getAllDivisionsOfSeasonByLevel(Long seasonId) {
        Collection<Division> divisions = divisionRepository.findBySeasonId(seasonId);
        SortedSetMultimap<Integer, DivisionData> divisionsOfSeasonByLevel = TreeMultimap.create(
                Integer::compareTo, Comparator.comparing(DivisionData::name));
        for (Division division : divisions) {
            divisionsOfSeasonByLevel.put(division.getLevel(), divisionToDivisionData(division));
        }
        return divisionsOfSeasonByLevel;
    }

    public SortedSetMultimap<Integer, DivisionData> getAllDivisionsOfSeason(String seasonName) {
        Long seasonId = seasonService.getSeasonIdByName(seasonName);
        return getAllDivisionsOfSeasonByLevel(seasonId);
    }

    private DivisionData divisionToDivisionData(Division division) {
        return new DivisionData(division.getId(),
                division.getName(),
                division.getLevel(),
                division.getNumberOfBoards(),
                seasonService.getNonArchivedSeasonById(division.getSeasonId()));
    }

}
