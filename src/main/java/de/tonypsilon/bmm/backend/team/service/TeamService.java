package de.tonypsilon.bmm.backend.team.service;

import de.tonypsilon.bmm.backend.exception.BadDataException;
import de.tonypsilon.bmm.backend.exception.NotFoundException;
import de.tonypsilon.bmm.backend.exception.SeasonStageException;
import de.tonypsilon.bmm.backend.organization.service.OrganizationService;
import de.tonypsilon.bmm.backend.season.service.SeasonService;
import de.tonypsilon.bmm.backend.season.service.SeasonStage;
import de.tonypsilon.bmm.backend.team.data.Team;
import de.tonypsilon.bmm.backend.team.data.TeamCreationData;
import de.tonypsilon.bmm.backend.team.data.TeamData;
import de.tonypsilon.bmm.backend.team.data.TeamRepository;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Service
public class TeamService {

    private final TeamRepository teamRepository;
    private final SeasonService seasonService;
    private final OrganizationService organizationService;

    public TeamService(final TeamRepository teamRepository,
                       SeasonService seasonService,
                       OrganizationService organizationService) {
        this.teamRepository = teamRepository;
        this.seasonService = seasonService;
        this.organizationService = organizationService;
    }

    @Transactional
    public TeamData createTeam(TeamCreationData teamCreationData) {
        if(!seasonService.seasonExistsById(teamCreationData.seasonId())) {
            throw new NotFoundException("Es gibt keine Saison mit ID %d!".formatted(teamCreationData.seasonId()));
        }
        if (!seasonService.getStageOfSeason(teamCreationData.seasonId()).equals(SeasonStage.REGISTRATION)) {
            throw new SeasonStageException("Saison ist nicht in der Registrierungsphase!");
        }
        if(!organizationService.existsByIdAndSeasonId(teamCreationData.organizationId(),
                teamCreationData.seasonId())) {
            throw new NotFoundException("Es gibt keine Organisation mit ID %d für die Saison mit der ID %d!"
                    .formatted(teamCreationData.organizationId(), teamCreationData.seasonId()));
        }
        verifyTeamNumber(teamCreationData);

        Team team = new Team();
        team.setSeasonId(teamCreationData.seasonId());
        team.setOrganizationId(teamCreationData.organizationId());
        team.setNumber(teamCreationData.number());

        teamRepository.save(team);

        return teamToTeamData(teamRepository.getBySeasonIdAndOrganizationIdAndNumber(teamCreationData.seasonId(),
                teamCreationData.organizationId(),
                teamCreationData.number()));
    }

    /**
     * Only to be called from another service after deletion
     * of the players of the team has been taken care of!
     * @param teamId
     */
    @Transactional
    public void deleteTeam(Long teamId) {
        Team team = teamRepository.findById(teamId).orElseThrow(
                () -> new NotFoundException("Es gibt kein Team mit ID %d!".formatted(teamId))
        );
        if (!seasonService.getStageOfSeason(team.getSeasonId()).equals(SeasonStage.REGISTRATION)) {
            throw new SeasonStageException("Saison ist nicht in der Registrierungsphase!");
        }
        if(!team.getNumber().equals(getMaxTeamNumberForTeamsOfOrganization(team.getSeasonId(), team.getOrganizationId()))) {
            throw new BadDataException(
                    "Es kann nur das Team mit der höchsten Nummer gelöscht werden!"
            );
        }
        teamRepository.delete(team);
    }

    public Boolean existsById(Long teamId) {
        return teamRepository.existsById(teamId);
    }

    public TeamData getTeamById(Long teamId) {
        return teamToTeamData(teamRepository.findById(teamId).orElseThrow(
                () -> new NotFoundException("Es gibt keine Mannschaft mit der ID %d".formatted(teamId))));
    }

    private void verifyTeamNumber(TeamCreationData teamCreationData) {
        Integer maxTeamNumber = getMaxTeamNumberForTeamsOfOrganization(teamCreationData.seasonId(), teamCreationData.organizationId());
        if (!teamCreationData.number().equals(maxTeamNumber+1)) {
            throw new BadDataException(
                    "Das neue Team hat nicht die passende Teamnummer. Erwartet: %d. Tatsächlich: %d."
                            .formatted(maxTeamNumber+1, teamCreationData.number()));
        }
    }

    private Integer getMaxTeamNumberForTeamsOfOrganization(Long seasonId, Long organizationId) {
        List<Integer> teamNumbers = teamRepository.findBySeasonIdAndOrganizationId(
                        seasonId, organizationId)
                .stream()
                .map(Team::getNumber)
                .toList();
        // TODO Log if team number sequence is not good.
        return teamNumbers.stream().max(Integer::compareTo).orElse(0);
    }

    private TeamData teamToTeamData(Team team) {
        return new TeamData(team.getId(),
                team.getSeasonId(),
                team.getOrganizationId(),
                team.getNumber());
    }
}
