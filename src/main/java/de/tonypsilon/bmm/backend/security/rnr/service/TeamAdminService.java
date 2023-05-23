package de.tonypsilon.bmm.backend.security.rnr.service;

import de.tonypsilon.bmm.backend.exception.AlreadyExistsException;
import de.tonypsilon.bmm.backend.exception.NotFoundException;
import de.tonypsilon.bmm.backend.organization.service.OrganizationService;
import de.tonypsilon.bmm.backend.security.rnr.data.TeamAdmin;
import de.tonypsilon.bmm.backend.security.rnr.data.TeamAdminData;
import de.tonypsilon.bmm.backend.security.rnr.data.TeamAdminRepository;
import de.tonypsilon.bmm.backend.team.data.TeamData;
import de.tonypsilon.bmm.backend.team.service.TeamService;
import org.springframework.lang.NonNull;
import org.springframework.security.provisioning.UserDetailsManager;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Comparator;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

@Service
public class TeamAdminService {

    private final TeamAdminRepository teamAdminRepository;
    private final TeamService teamService;
    private final OrganizationService organizationService;
    private final UserDetailsManager userDetailsManager;

    public TeamAdminService(final TeamAdminRepository teamAdminRepository,
                            final TeamService teamService,
                            final OrganizationService organizationService,
                            final UserDetailsManager userDetailsManager) {
        this.teamAdminRepository = teamAdminRepository;
        this.teamService = teamService;
        this.organizationService = organizationService;
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
    public void deleteTeamAdmin(@NonNull TeamAdminData teamAdminData) {
        TeamAdmin teamAdminToDelete = teamAdminRepository.findByTeamIdAndUsername(teamAdminData.teamId(), teamAdminData.username())
                .orElseThrow(() -> new NotFoundException("Benutzer %s ist kein Administrator für das Team mit ID %d!"
                        .formatted(teamAdminData.username(), teamAdminData.teamId())));
        teamAdminRepository.delete(teamAdminToDelete);

    }

    @NonNull
    @Transactional
    public Set<TeamAdminData> getTeamAdminsOfTeam(@NonNull Long teamId) {
        return teamAdminRepository.findByTeamId(teamId).stream()
                .map(this::teamAdminToTeamAdminData)
                .collect(Collectors.toSet());
    }

    @NonNull
    public List<TeamData> getTeamsOfTeamAdmin(@NonNull String username) {
        return teamAdminRepository.findByUsername(username).stream()
                .map(TeamAdmin::getTeamId)
                .map(teamService::getTeamDataById)
                .sorted(Comparator.comparing(this::teamDataToTeamName))
                .toList();
    }

    private String teamDataToTeamName(TeamData teamData) {
        return organizationService.getOrganizationById(teamData.organizationId()).name()
                + " " + teamData.number();
    }

    @NonNull
    private TeamAdminData teamAdminToTeamAdminData(@NonNull TeamAdmin teamAdmin) {
        return new TeamAdminData(teamAdmin.getTeamId(), teamAdmin.getUsername());
    }
}
