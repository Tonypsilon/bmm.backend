package de.tonypsilon.bmm.backend.participationeligibility.service;

import de.tonypsilon.bmm.backend.club.service.ClubService;
import de.tonypsilon.bmm.backend.exception.AlreadyExistsException;
import de.tonypsilon.bmm.backend.exception.BadDataException;
import de.tonypsilon.bmm.backend.exception.NotFoundException;
import de.tonypsilon.bmm.backend.participationeligibility.data.ParticipationEligibility;
import de.tonypsilon.bmm.backend.participationeligibility.data.ParticipationEligibilityCreationData;
import de.tonypsilon.bmm.backend.participationeligibility.data.ParticipationEligibilityData;
import de.tonypsilon.bmm.backend.participationeligibility.data.ParticipationEligibilityRepository;
import de.tonypsilon.bmm.backend.season.service.SeasonService;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Collection;

@Service
public class ParticipationEligibilityService {

    private final ParticipationEligibilityRepository participationEligibilityRepository;
    private final SeasonService seasonService;
    private final ClubService clubService;

    public ParticipationEligibilityService(final ParticipationEligibilityRepository participationEligibilityRepository,
                                           final SeasonService seasonService,
                                           final ClubService clubService) {
        this.participationEligibilityRepository = participationEligibilityRepository;
        this.seasonService = seasonService;
        this.clubService = clubService;
    }

    @Transactional
    public ParticipationEligibilityData createParticipationEligibility(
            ParticipationEligibilityCreationData participationEligibilityCreationData) {
        if (!seasonService.seasonExistsById(participationEligibilityCreationData.seasonId())) {
            throw new NotFoundException("Es gibt keine Saison mit der ID %d!"
                    .formatted(participationEligibilityCreationData.seasonId()));
        }
        if (!clubService.clubExistsById(participationEligibilityCreationData.clubId())) {
            throw new NotFoundException("Es gibt keinen Verein mit der ID %d!"
                    .formatted(participationEligibilityCreationData.clubId()));
        }
        verifyName(participationEligibilityCreationData.forename());
        verifyName(participationEligibilityCreationData.surname());
        participationEligibilityCreationData.dwz().ifPresent(this::verifyDwz);
        if (participationEligibilityRepository.existsBySeasonIdAndClubIdAndPkz(
                participationEligibilityCreationData.seasonId(),
                participationEligibilityCreationData.clubId(),
                participationEligibilityCreationData.pkz())) {
            throw new AlreadyExistsException(("Es gibt bereits eine Spielberechtigung f??r die Spielernummer %d" +
                    " f??r den Club mit ID %d und die Saison mit der ID %d!")
                    .formatted(participationEligibilityCreationData.pkz(),
                            participationEligibilityCreationData.clubId(),
                            participationEligibilityCreationData.seasonId()));
        }
        ParticipationEligibility participationEligibility = new ParticipationEligibility();
        participationEligibility.setSeasonId(participationEligibilityCreationData.seasonId());
        participationEligibility.setClubId(participationEligibilityCreationData.clubId());
        participationEligibility.setForename(participationEligibility.getForename());
        participationEligibility.setSurname(participationEligibility.getSurname());
        participationEligibility.setPkz(participationEligibilityCreationData.pkz());
        participationEligibility.setDwz(participationEligibilityCreationData.dwz().orElse(null));
        participationEligibilityRepository.save(participationEligibility);

        return participationEligibilityToParticipationEligibilityData(
                participationEligibilityRepository.getBySeasonIdAndClubIdAndPkz(participationEligibility.getSeasonId(),
                        participationEligibility.getClubId(),
                        participationEligibility.getPkz()));
    }

    public Collection<ParticipationEligibilityData> getAllParticipationEligibilitiesForSeason(Long seasonId) {
        return participationEligibilityRepository
                .getBySeasonId(seasonId)
                .stream()
                .map(this::participationEligibilityToParticipationEligibilityData)
                .toList();
    }

    public Collection<ParticipationEligibilityData> getAllParticipationEligibilitiesForSeasonAndClub(
            Long seasonId, Long clubId) {
        return participationEligibilityRepository
                .getBySeasonIdAndClubId(seasonId, clubId)
                .stream()
                .map(this::participationEligibilityToParticipationEligibilityData)
                .toList();
    }

    @Transactional
    public void deleteParticipationEligibility(Long participationEligibilityId) {
        participationEligibilityRepository.delete(
                participationEligibilityRepository.findById(participationEligibilityId)
                        .orElseThrow(() -> new NotFoundException("Es gibt keine Spielberechtigung mit der ID %d!"
                                .formatted(participationEligibilityId))));
    }

    @NonNull
    private ParticipationEligibilityData participationEligibilityToParticipationEligibilityData(
            @NonNull ParticipationEligibility participationEligibility) {
        return new ParticipationEligibilityData(participationEligibility.getId(),
                participationEligibility.getSeasonId(),
                participationEligibility.getClubId(),
                participationEligibility.getForename(),
                participationEligibility.getSurname(),
                participationEligibility.getPkz(),
                participationEligibility.getDwz());
    }

    private void verifyDwz(Integer dwz) {
        if (dwz <= 0) {
            throw new BadDataException("Die DWZ muss positiv sein!");
        }
    }

    private void verifyName(String name) {
        if (name == null || name.isBlank()) {
            throw new BadDataException("Der Name darf nicht leer sein!");
        }
    }
}
