package de.tonypsilon.bmm.backend.referee.service;

import de.tonypsilon.bmm.backend.referee.data.RefereeRepository;
import org.springframework.stereotype.Service;

@Service
public class RefereeService {

    private final RefereeRepository refereeRepository;

    public RefereeService(final RefereeRepository refereeRepository) {
        this.refereeRepository = refereeRepository;
    }
}
