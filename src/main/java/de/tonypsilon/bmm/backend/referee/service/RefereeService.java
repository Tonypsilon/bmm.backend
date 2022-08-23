package de.tonypsilon.bmm.backend.referee.service;

import de.tonypsilon.bmm.backend.referee.data.Referee;
import de.tonypsilon.bmm.backend.referee.data.RefereeData;
import de.tonypsilon.bmm.backend.referee.data.RefereeRepository;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Service;

import java.util.Optional;

@Service
public class RefereeService {

    private final RefereeRepository refereeRepository;

    public RefereeService(final RefereeRepository refereeRepository) {
        this.refereeRepository = refereeRepository;
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
}
