package de.tonypsilon.bmm.backend.referee.service;

import de.tonypsilon.bmm.backend.exception.*;
import de.tonypsilon.bmm.backend.referee.data.*;
import de.tonypsilon.bmm.backend.referee.facade.RefereeExternalData;
import de.tonypsilon.bmm.backend.season.service.SeasonService;
import de.tonypsilon.bmm.backend.season.service.SeasonStage;
import de.tonypsilon.bmm.backend.validation.service.ValidationService;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Optional;

@Service
public class RefereeService {

    private final RefereeRepository refereeRepository;
    private final SeasonService seasonService;
    private final ValidationService validationService;

    public RefereeService(final RefereeRepository refereeRepository,
                          final SeasonService seasonService,
                          final ValidationService validationService) {
        this.refereeRepository = refereeRepository;
        this.seasonService = seasonService;
        this.validationService = validationService;
    }

    @Transactional
    @NonNull
    public RefereeData createReferee(RefereeCreationData refereeCreationData) {
        if(!seasonService.seasonExistsById(refereeCreationData.seasonId())) {
            throw new NotFoundException("Es gibt keine Saison mit der ID %d!"
                    .formatted(refereeCreationData.seasonId()));
        }
        if(Boolean.TRUE.equals(refereeRepository.existsBySeasonIdAndEmailAddress(
                refereeCreationData.seasonId(), refereeCreationData.emailAddress()))) {
            throw new AlreadyExistsException("Es gibt bereits einen Schiedsrichter für die Saison mit der ID %d"
                    .formatted(refereeCreationData.seasonId())
            + " und der E-Mailadresse %s!".formatted(refereeCreationData.emailAddress()));
        }
        validationService.validateName(refereeCreationData.forename());
        validationService.validateName(refereeCreationData.surname());
        validationService.validateEmailAddress(refereeCreationData.emailAddress());

        Referee referee = new Referee();
        referee.setSeasonId(refereeCreationData.seasonId());
        referee.setForename(refereeCreationData.forename());
        referee.setSurname(refereeCreationData.surname());
        referee.setEmailAddress(refereeCreationData.emailAddress());
        refereeRepository.save(referee);

        return refereeToRefereeData(
                refereeRepository.getBySeasonIdAndEmailAddress(
                        refereeCreationData.seasonId(), refereeCreationData.emailAddress()));
    }

    @Transactional
    @NonNull
    public RefereeData updateReferee(RefereeData refereeUpdateData) {
        Referee refereeToBeUpdated = refereeRepository.findById(refereeUpdateData.id())
                .orElseThrow(() -> new NotFoundException("Es gibt keinen Schiedsrichter mit der ID %d!"
                        .formatted(refereeUpdateData.id())));
        if(!refereeUpdateData.seasonId().equals(refereeToBeUpdated.getSeasonId())) {
            throw new BadDataException("Die Saison darf sich nicht ändern!");
        }
        if(!refereeToBeUpdated.getEmailAddress().equals(refereeUpdateData.emailAddress()) &&
                Boolean.TRUE.equals(refereeRepository.existsBySeasonIdAndEmailAddress(
                refereeUpdateData.seasonId(), refereeUpdateData.emailAddress()))) {
            throw new AlreadyExistsException("Es gibt bereits einen Schiedsrichter für die Saison mit der ID %d"
                    .formatted(refereeUpdateData.seasonId())
                    + " und der E-Mailadresse %s!".formatted(refereeUpdateData.emailAddress()));
        }
        validationService.validateName(refereeUpdateData.forename());
        validationService.validateName(refereeUpdateData.surname());
        validationService.validateEmailAddress(refereeUpdateData.emailAddress());

        refereeToBeUpdated.setForename(refereeUpdateData.forename());
        refereeToBeUpdated.setSurname(refereeUpdateData.surname());
        refereeToBeUpdated.setEmailAddress(refereeUpdateData.emailAddress());

        refereeRepository.save(refereeToBeUpdated);

        return refereeToRefereeData(
                refereeRepository.getBySeasonIdAndEmailAddress(
                        refereeUpdateData.seasonId(), refereeUpdateData.emailAddress()));
    }

    @Transactional
    public void deleteReferee(RefereeData refereeDeleteData) {
        Referee refereeToBeDeleted = refereeRepository.findById(refereeDeleteData.id())
                .orElseThrow(() -> new NotFoundException("Es gibt keinen Schiedsrichter mit der ID %d!"
                        .formatted(refereeDeleteData.id())));
        if(!List.of(SeasonStage.REGISTRATION, SeasonStage.PREPARATION).contains(
                seasonService.getStageOfSeason(refereeDeleteData.seasonId()))) {
            throw new SeasonStageException("In dieser Saisonphase kann kein Schiedsrichter gelöscht werden!");
        }
        refereeRepository.delete(refereeToBeDeleted);
    }

    public Optional<RefereeData> findById(@NonNull Long refereeId) {
        return refereeRepository.findById(refereeId)
                .map(this::refereeToRefereeData);
    }

    @NonNull
    public RefereeData getById(@NonNull Long refereeId) {
        return this.findById(refereeId).orElseThrow(() ->
                new NotFoundException("Es gibt keinen Schiedsrichter mit der ID %d".formatted(refereeId)));
    }

    @NonNull
    public List<RefereeData> findBySeasonId(@NonNull Long seasonId) {
        return refereeRepository.findBySeasonIdOrderBySurnameAsc(seasonId).stream()
                .map(this::refereeToRefereeData)
                .toList();
    }

    @NonNull
    private RefereeData refereeToRefereeData(@NonNull Referee referee) {
        return new RefereeData(referee.getId(),
                referee.getSeasonId(),
                referee.getForename(),
                referee.getSurname(),
                referee.getEmailAddress());
    }

    @NonNull
    public RefereeExternalData refereeDataToRefereeExternalData(@NonNull RefereeData refereeData) {
        return new RefereeExternalData(refereeData.id(),
                refereeData.seasonId(),
                refereeData.forename(),
                refereeData.surname());
    }
}
