package de.tonypsilon.bmm.backend.security.rnr.service;

import de.tonypsilon.bmm.backend.security.rnr.data.TeamAdminRepository;
import org.springframework.stereotype.Service;

@Service
public class TeamAdminService {

    private final TeamAdminRepository teamAdminRepository;

    public TeamAdminService(final TeamAdminRepository teamAdminRepository) {
        this.teamAdminRepository = teamAdminRepository;
    }
}
