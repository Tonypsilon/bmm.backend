package de.tonypsilon.bmm.backend.teamcaptain.service;

import de.tonypsilon.bmm.backend.teamcaptain.data.TeamCaptainRepository;
import org.springframework.stereotype.Service;

@Service
public class TeamCaptainService {

    private final TeamCaptainRepository teamCaptainRepository;

    public TeamCaptainService(final TeamCaptainRepository teamCaptainRepository) {
        this.teamCaptainRepository = teamCaptainRepository;
    }

}
