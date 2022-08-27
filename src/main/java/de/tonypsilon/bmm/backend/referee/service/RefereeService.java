package de.tonypsilon.bmm.backend.referee.service;

import de.tonypsilon.bmm.backend.exception.*;
import de.tonypsilon.bmm.backend.referee.data.*;
import de.tonypsilon.bmm.backend.season.service.SeasonService;
import de.tonypsilon.bmm.backend.season.service.SeasonStage;
import org.apache.commons.validator.routines.EmailValidator;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Optional;

@Service
public class RefereeService {

    private final RefereeRepository refereeRepository;
    private final SeasonService seasonService;
    private final EmailValidator emailValidator = EmailValidator.getInstance();

    public RefereeService(final RefereeRepository refereeRepository,
                          final SeasonService seasonService) {
        this.refereeRepository = refereeRepository;
        this.seasonService = seasonService;
    }

    @Transactional
    @NonNull
    public RefereeData createReferee(CreateRefereeData createRefereeData) {
        if(!seasonService.seasonExistsById(createRefereeData.seasonId())) {
            throw new NotFoundException("Es gibt keine Saison mit der ID %d!"
                    .formatted(createRefereeData.seasonId()));
        }
        if(refereeRepository.existsBySeasonIdAndEmailAddress(
                createRefereeData.seasonId(), createRefereeData.emailAddress())) {
            throw new AlreadyExistsException("Es gibt bereits einen Schiedsrichter für die Saison mit der ID %d"
                    .formatted(createRefereeData.seasonId())
            + " und der E-Mailadresse %s!".formatted(createRefereeData.emailAddress()));
        }
        verifyName(createRefereeData.forename());
        verifyName(createRefereeData.surname());
        verifyEmailAddress(createRefereeData.emailAddress());

        Referee referee = new Referee();
        referee.setSeasonId(createRefereeData.seasonId());
        referee.setForename(createRefereeData.forename());
        referee.setSurname(createRefereeData.surname());
        referee.setEmailAddress(createRefereeData.emailAddress());
        refereeRepository.save(referee);

        return refereeToRefereeData(
                refereeRepository.getBySeasonIdAndEmailAddress(
                        createRefereeData.seasonId(), createRefereeData.emailAddress()));
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
        if(refereeRepository.existsBySeasonIdAndEmailAddress(
                refereeUpdateData.seasonId(), refereeUpdateData.emailAddress())) {
            throw new AlreadyExistsException("Es gibt bereits einen Schiedsrichter für die Saison mit der ID %d"
                    .formatted(refereeUpdateData.seasonId())
                    + " und der E-Mailadresse %s!".formatted(refereeUpdateData.emailAddress()));
        }
        verifyName(refereeUpdateData.forename());
        verifyName(refereeUpdateData.surname());
        verifyEmailAddress(refereeUpdateData.emailAddress());

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
    private RefereeData refereeToRefereeData(@NonNull Referee referee) {
        return new RefereeData(referee.getId(),
                referee.getSeasonId(),
                referee.getForename(),
                referee.getSurname(),
                referee.getEmailAddress());
    }

    private void verifyName(String name) {
        if (name == null || name.isBlank()) {
            throw new BadDataException("Der Name darf nicht leer sein!");
        }

    }

    private void verifyEmailAddress(String emailAddress) {
        if(emailAddress == null || !emailValidator.isValid(emailAddress)) {
            throw new BadDataException("Die E-Mailadresse ist ungültig!");
        }
    }
}
