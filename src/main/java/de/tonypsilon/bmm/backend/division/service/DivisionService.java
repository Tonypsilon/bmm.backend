package de.tonypsilon.bmm.backend.division.service;

import com.google.common.collect.SortedSetMultimap;
import com.google.common.collect.TreeMultimap;
import de.tonypsilon.bmm.backend.division.data.Division;
import de.tonypsilon.bmm.backend.division.data.DivisionCreationData;
import de.tonypsilon.bmm.backend.division.data.DivisionData;
import de.tonypsilon.bmm.backend.division.data.DivisionRepository;
import de.tonypsilon.bmm.backend.exception.AlreadyExistsException;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Collection;
import java.util.Comparator;

@Service
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

    @Transactional
    public DivisionData createDivision(DivisionCreationData divisionCreationData) {
        if (divisionRepository.existsBySeasonIdAndName(divisionCreationData.seasonId(), divisionCreationData.name())) {
            throw new AlreadyExistsException("Staffel mit Namen %s für Saison mit ID %d existiert bereits!"
                    .formatted(divisionCreationData.name(), divisionCreationData.seasonId()));
        }
        Division division = new Division();
        division.setName(divisionCreationData.name());
        division.setSeasonId(divisionCreationData.seasonId());
        division.setLevel(divisionCreationData.level());
        division.setNumberOfBoards(divisionCreationData.numberOfBoards());
        divisionRepository.save(division);
        return divisionToDivisionData(
                divisionRepository.getBySeasonIdAndName(divisionCreationData.seasonId(), divisionCreationData.name()));
    }

    @NonNull
    private DivisionData divisionToDivisionData(@NonNull Division division) {
        return new DivisionData(division.getId(),
                division.getName(),
                division.getLevel(),
                division.getNumberOfBoards(),
                division.getSeasonId());
    }

}
