package de.tonypsilon.bmm.backend.teamcaptain.service;

import de.tonypsilon.bmm.backend.exception.NotFoundException;
import de.tonypsilon.bmm.backend.team.service.TeamService;
import de.tonypsilon.bmm.backend.teamcaptain.data.CreateTeamCaptainData;
import de.tonypsilon.bmm.backend.teamcaptain.data.TeamCaptainData;
import de.tonypsilon.bmm.backend.teamcaptain.data.TeamCaptainRepository;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
public class TeamCaptainService {

    private final TeamCaptainRepository teamCaptainRepository;
    private final TeamService teamService;

    public TeamCaptainService(final TeamCaptainRepository teamCaptainRepository,
                              final TeamService teamService) {
        this.teamCaptainRepository = teamCaptainRepository;
        this.teamService = teamService;
    }

    @Transactional
    @NonNull
    public TeamCaptainData createTeamCaptain(CreateTeamCaptainData createTeamCaptainData) {
        if(!teamService.existsById(createTeamCaptainData.teamId())) {
            throw new NotFoundException("Es gibt keine Mannschaft mit der ID %d!"
                    .formatted(createTeamCaptainData.teamId()));
        }

        return null;
    }

}
