package de.tonypsilon.bmm.backend.participationeligibility.service;

import de.tonypsilon.bmm.backend.club.service.ClubService;
import de.tonypsilon.bmm.backend.exception.AlreadyExistsException;
import de.tonypsilon.bmm.backend.exception.NotFoundException;
import de.tonypsilon.bmm.backend.exception.SeasonStageException;
import de.tonypsilon.bmm.backend.organization.data.OrganizationData;
import de.tonypsilon.bmm.backend.participationeligibility.data.*;
import de.tonypsilon.bmm.backend.season.service.SeasonService;
import de.tonypsilon.bmm.backend.season.service.SeasonStage;
import de.tonypsilon.bmm.backend.validation.service.ValidationService;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Collection;
import java.util.Optional;
import java.util.function.Predicate;

@Service
public class ParticipationEligibilityService {

    private final ParticipationEligibilityRepository participationEligibilityRepository;
    private final SeasonService seasonService;
    private final ClubService clubService;
    private final ValidationService validationService;

    public ParticipationEligibilityService(final ParticipationEligibilityRepository participationEligibilityRepository,
                                           final SeasonService seasonService,
                                           final ClubService clubService,
                                           final ValidationService validationService) {
        this.participationEligibilityRepository = participationEligibilityRepository;
        this.seasonService = seasonService;
        this.clubService = clubService;
        this.validationService = validationService;
    }

    @Transactional
    @NonNull
    public ParticipationEligibilityData createParticipationEligibility(
            ParticipationEligibilityCreationData participationEligibilityCreationData) {
        if (Boolean.FALSE.equals(seasonService.seasonExistsById(participationEligibilityCreationData.seasonId()))) {
            throw new NotFoundException("Es gibt keine Saison mit der ID %d!"
                    .formatted(participationEligibilityCreationData.seasonId()));
        }
        if(seasonService.getStageOfSeason(participationEligibilityCreationData.seasonId()) != SeasonStage.REGISTRATION) {
            throw new SeasonStageException("Die Saison ist nicht in der Registrierungsphase!");
        }
        if (Boolean.FALSE.equals(clubService.clubExistsById(participationEligibilityCreationData.clubId()))) {
            throw new NotFoundException("Es gibt keinen Verein mit der ID %d!"
                    .formatted(participationEligibilityCreationData.clubId()));
        }
        validationService.validateName(participationEligibilityCreationData.forename());
        validationService.validateName(participationEligibilityCreationData.surname());
        Optional<Integer> dwz = Optional.ofNullable(participationEligibilityCreationData.dwz());
        dwz.ifPresent(validationService::validateRating);
        if (Boolean.TRUE.equals(participationEligibilityRepository.existsBySeasonIdAndClubIdAndPkz(
                participationEligibilityCreationData.seasonId(),
                participationEligibilityCreationData.clubId(),
                participationEligibilityCreationData.pkz()))) {
            throw new AlreadyExistsException(("Es gibt bereits eine Spielberechtigung für die Spielernummer %d" +
                    " für den Club mit ID %d und die Saison mit der ID %d!")
                    .formatted(participationEligibilityCreationData.pkz(),
                            participationEligibilityCreationData.clubId(),
                            participationEligibilityCreationData.seasonId()));
        }
        ParticipationEligibility participationEligibility = new ParticipationEligibility();
        participationEligibility.setSeasonId(participationEligibilityCreationData.seasonId());
        participationEligibility.setClubId(participationEligibilityCreationData.clubId());
        participationEligibility.setForename(participationEligibilityCreationData.forename());
        participationEligibility.setSurname(participationEligibilityCreationData.surname());
        participationEligibility.setPkz(participationEligibilityCreationData.pkz());
        participationEligibility.setDwz(dwz.orElse(null));
        participationEligibilityRepository.save(participationEligibility);

        return participationEligibilityToParticipationEligibilityData(
                participationEligibilityRepository.getBySeasonIdAndClubIdAndPkz(participationEligibility.getSeasonId(),
                        participationEligibility.getClubId(),
                        participationEligibility.getPkz()));
    }

    @NonNull
    public Collection<ParticipationEligibilityData> getAllParticipationEligibilitiesForSeason(@NonNull Long seasonId) {
        return participationEligibilityRepository
                .getBySeasonId(seasonId)
                .stream()
                .map(this::participationEligibilityToParticipationEligibilityData)
                .toList();
    }

    @NonNull
    public Collection<ParticipationEligibilityData> getAllParticipationEligibilitiesForSeasonAndClub(
            @NonNull Long seasonId, @NonNull Long clubId) {
        return participationEligibilityRepository
                .getBySeasonIdAndClubId(seasonId, clubId)
                .stream()
                .map(this::participationEligibilityToParticipationEligibilityData)
                .toList();
    }

    @Transactional
    public void deleteParticipationEligibility(@NonNull Long participationEligibilityId) {
        participationEligibilityRepository.delete(
                participationEligibilityRepository.findById(participationEligibilityId)
                        .orElseThrow(() -> new NotFoundException("Es gibt keine Spielberechtigung mit der ID %d!"
                                .formatted(participationEligibilityId))));
    }

    @NonNull
    public Boolean existsById(@NonNull Long participationEligibilityId) {
        return participationEligibilityRepository.existsById(participationEligibilityId);
    }

    @NonNull
    public Predicate<Long> isValidParticipationEligibilityForOrganization(OrganizationData organizationData) {
        return participationEligibilityId -> {
            ParticipationEligibility participationEligibility = getById(participationEligibilityId);
            return organizationData.clubIds().contains(participationEligibility.getClubId())
                    && organizationData.seasonId().equals(participationEligibility.getSeasonId());
        };
    }

    public ParticipationEligibilityData getParticipationEligibilityById(Long participationEligibilityId) {
        return participationEligibilityToParticipationEligibilityData(getById(participationEligibilityId));
    }

    private ParticipationEligibility getById(Long participationEligibilityId) {
        return participationEligibilityRepository.findById(participationEligibilityId)
                .orElseThrow(() -> new NotFoundException("Es gibt keine Spielberechtigung mit der ID %d!"
                        .formatted(participationEligibilityId)));
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
                participationEligibility.getDwz().orElse(null));
    }

}
