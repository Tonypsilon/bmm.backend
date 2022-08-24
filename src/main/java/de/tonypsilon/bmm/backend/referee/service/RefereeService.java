package de.tonypsilon.bmm.backend.referee.service;

import de.tonypsilon.bmm.backend.exception.AlreadyExistsException;
import de.tonypsilon.bmm.backend.exception.BadDataException;
import de.tonypsilon.bmm.backend.exception.NotFoundException;
import de.tonypsilon.bmm.backend.referee.data.*;
import de.tonypsilon.bmm.backend.season.service.SeasonService;
import org.apache.commons.validator.routines.EmailValidator;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

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
