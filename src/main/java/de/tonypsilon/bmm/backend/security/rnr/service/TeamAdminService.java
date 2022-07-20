package de.tonypsilon.bmm.backend.security.rnr.service;

import de.tonypsilon.bmm.backend.exception.AlreadyExistsException;
import de.tonypsilon.bmm.backend.exception.NotFoundException;
import de.tonypsilon.bmm.backend.security.rnr.data.TeamAdmin;
import de.tonypsilon.bmm.backend.security.rnr.data.TeamAdminData;
import de.tonypsilon.bmm.backend.security.rnr.data.TeamAdminRepository;
import de.tonypsilon.bmm.backend.team.service.TeamService;
import org.springframework.lang.NonNull;
import org.springframework.security.provisioning.UserDetailsManager;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
public class TeamAdminService {

    private final TeamAdminRepository teamAdminRepository;
    private final TeamService teamService;
    private final UserDetailsManager userDetailsManager;

    public TeamAdminService(final TeamAdminRepository teamAdminRepository,
                            final TeamService teamService,
                            final UserDetailsManager userDetailsManager) {
        this.teamAdminRepository = teamAdminRepository;
        this.teamService = teamService;
        this.userDetailsManager = userDetailsManager;
    }

    @Transactional
    public TeamAdminData createTeamAdmin(TeamAdminData teamAdminCreateData) {
        if(!userDetailsManager.userExists(teamAdminCreateData.username())) {
            throw new NotFoundException("Es gibt keinen Benutzer mit dem Namen %s!".formatted(teamAdminCreateData.username()));
        }
        if(!teamService.existsById(teamAdminCreateData.teamId())) {
            throw new NotFoundException("Es gibt kein Team mit der ID %d!".formatted(teamAdminCreateData.teamId()));
        }
        if(teamAdminRepository.existsByTeamIdAndUsername(teamAdminCreateData.teamId(), teamAdminCreateData.username())) {
            throw new AlreadyExistsException("Benutzer %s ist bereits Administrator für das Team mit ID %d!"
                    .formatted(teamAdminCreateData.username(), teamAdminCreateData.teamId()));
        }
        TeamAdmin teamAdmin = new TeamAdmin();
        teamAdmin.setTeamId(teamAdminCreateData.teamId());
        teamAdmin.setUsername(teamAdminCreateData.username());
        teamAdminRepository.save(teamAdmin);
        return teamAdminToTeamAdminData(teamAdminRepository.getByTeamIdAndUsername(
                teamAdminCreateData.teamId(), teamAdminCreateData.username()));
    }

    @NonNull
    public Boolean isTeamAdmin(@NonNull Long teamId, @NonNull String username) {
        return teamAdminRepository.existsByTeamIdAndUsername(teamId, username);
    }

    @Transactional
    public void deleteTeamAdmin(Long teamId, String username) {
        TeamAdmin teamAdminToDelete = teamAdminRepository.findByTeamIdAndUsername(teamId, username)
                .orElseThrow(() -> new NotFoundException("Benutzer %s ist kein Administrator für das Team mit ID %d!"
                        .formatted(username, teamId)));
        teamAdminRepository.delete(teamAdminToDelete);

    }

    @NonNull
    private TeamAdminData teamAdminToTeamAdminData(@NonNull TeamAdmin teamAdmin) {
        return new TeamAdminData(teamAdmin.getTeamId(), teamAdmin.getUsername());
    }
}
