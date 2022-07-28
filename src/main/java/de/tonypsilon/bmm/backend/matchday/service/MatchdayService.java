package de.tonypsilon.bmm.backend.matchday.service;

import de.tonypsilon.bmm.backend.matchday.data.MatchdayRepository;
import org.springframework.stereotype.Service;

@Service
public class MatchdayService {

    private final MatchdayRepository matchdayRepository;

    public MatchdayService(MatchdayRepository matchdayRepository) {
        this.matchdayRepository = matchdayRepository;
    }
}
