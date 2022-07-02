package de.tonypsilon.bmm.backend.division.service;

import com.google.common.collect.SortedSetMultimap;
import com.google.common.collect.TreeMultimap;
import de.tonypsilon.bmm.backend.division.data.Division;
import de.tonypsilon.bmm.backend.division.data.DivisionData;
import de.tonypsilon.bmm.backend.division.data.DivisionRepository;

import java.util.Collection;
import java.util.Comparator;

public class DivisionService {

    private final DivisionRepository divisionRepository;

    public DivisionService(DivisionRepository divisionRepository) {
        this.divisionRepository = divisionRepository;
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

    private DivisionData divisionToDivisionData(Division division) {
        return new DivisionData(division.getId(),
                division.getName(),
                division.getLevel(),
                division.getNumberOfBoards(),
                division.getSeasonId());
    }

}
