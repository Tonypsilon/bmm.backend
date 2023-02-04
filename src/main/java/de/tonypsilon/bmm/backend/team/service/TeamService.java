package de.tonypsilon.bmm.backend.team.service;

import de.tonypsilon.bmm.backend.division.service.DivisionService;
import de.tonypsilon.bmm.backend.exception.BadDataException;
import de.tonypsilon.bmm.backend.exception.NotFoundException;
import de.tonypsilon.bmm.backend.exception.SeasonStageException;
import de.tonypsilon.bmm.backend.organization.service.OrganizationService;
import de.tonypsilon.bmm.backend.season.service.SeasonService;
import de.tonypsilon.bmm.backend.season.service.SeasonStage;
import de.tonypsilon.bmm.backend.team.data.*;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Service
public class TeamService {

    private final TeamRepository teamRepository;
    private final SeasonService seasonService;
    private final DivisionService divisionService;
    private final OrganizationService organizationService;

    public TeamService(final TeamRepository teamRepository,
                       SeasonService seasonService,
                       DivisionService divisionService,
                       OrganizationService organizationService) {
        this.teamRepository = teamRepository;
        this.seasonService = seasonService;
        this.divisionService = divisionService;
        this.organizationService = organizationService;
    }

    @Transactional
    public TeamData createTeam(TeamCreationData teamCreationData) {
        // If the organization would not exist, getStageOfSeason would throw a NotFound Exception.
        // Hence, no need to explicitly check that again.
        if (!seasonService.getStageOfSeason(
                organizationService.getSeasonIdOfOrganization(teamCreationData.organizationId()))
                .equals(SeasonStage.REGISTRATION)) {
            throw new SeasonStageException("Saison ist nicht in der Registrierungsphase!");
        }

        verifyTeamNumber(teamCreationData);

        Team team = new Team();
        team.setOrganizationId(teamCreationData.organizationId());
        team.setNumber(teamCreationData.number());
        team.setDivisionId(null);

        teamRepository.save(team);

        return teamToTeamData(teamRepository.getByOrganizationIdAndNumber(
                teamCreationData.organizationId(),
                teamCreationData.number()));
    }

    /**
     * Only to be called from another service after deletion
     * of the participants of the team has been taken care of!
     * @param teamId
     */
    @Transactional
    public void deleteTeam(Long teamId) {
        Team team = teamRepository.findById(teamId).orElseThrow(
                () -> new NotFoundException("Es gibt kein Team mit ID %d!".formatted(teamId))
        );
        if (!seasonService.getStageOfSeason(
                organizationService.getSeasonIdOfOrganization(team.getOrganizationId())
        ).equals(SeasonStage.REGISTRATION)) {
            throw new SeasonStageException("Saison ist nicht in der Registrierungsphase!");
        }
        if(!team.getNumber().equals(getMaxTeamNumberForTeamsOfOrganization(team.getOrganizationId()))) {
            throw new BadDataException(
                    "Es kann nur das Team mit der höchsten Nummer gelöscht werden!"
            );
        }
        teamRepository.delete(team);
    }

    @NonNull
    public Boolean existsById(@NonNull Long teamId) {
        return teamRepository.existsById(teamId);
    }

    @NonNull
    public TeamData getTeamDataById(@NonNull Long teamId) {
        return teamToTeamData(getById(teamId));
    }

    @NonNull
    private Team getById(@NonNull Long teamId) {
        return teamRepository.findById(teamId).orElseThrow(
                () -> new NotFoundException("Es gibt keine Mannschaft mit der ID %d".formatted(teamId)));
    }

    /**
     * It is not checked if the team already is assigned to a division.
     * @param teamDivisionAssignmentData teamId and divisionId
     * @return teamData of the patched team
     */
    @NonNull
    public TeamData assignTeamToDivision(@NonNull TeamDivisionAssignmentData teamDivisionAssignmentData) {
        Team teamToBePatched = getById(teamDivisionAssignmentData.teamId());
        if(seasonService.getStageOfSeason(
                divisionService.getSeasonIdByDivisionId(teamDivisionAssignmentData.divisionId()))
                != SeasonStage.PREPARATION) {
            throw new SeasonStageException("Saison ist nicht in der Vorbereitungsphase!");
        }
        if(!divisionService.divisionExistsById(teamDivisionAssignmentData.divisionId())) {
            throw new BadDataException("Es gibt keine Staffel mit der ID %d!"
                    .formatted(teamDivisionAssignmentData.divisionId()));
        }
        teamToBePatched.setDivisionId(teamDivisionAssignmentData.divisionId());
        teamRepository.save(teamToBePatched);
        return getTeamDataById(teamDivisionAssignmentData.teamId());
    }

    @NonNull
    public Long getSeasonIdByTeamId(@NonNull Long teamId) {
        return organizationService.getSeasonIdOfOrganization(getById(teamId).getOrganizationId());
    }

    private void verifyTeamNumber(TeamCreationData teamCreationData) {
        Integer maxTeamNumber = getMaxTeamNumberForTeamsOfOrganization(teamCreationData.organizationId());
        if (!teamCreationData.number().equals(maxTeamNumber+1)) {
            throw new BadDataException(
                    "Das neue Team hat nicht die passende Teamnummer. Erwartet: %d. Tatsächlich: %d."
                            .formatted(maxTeamNumber+1, teamCreationData.number()));
        }
    }

    private Integer getMaxTeamNumberForTeamsOfOrganization(Long organizationId) {
        List<Integer> teamNumbers = teamRepository.findByOrganizationId(organizationId)
                .stream()
                .map(Team::getNumber)
                .toList();
        // TODO Log if team number sequence is not good.
        return teamNumbers.stream().max(Integer::compareTo).orElse(0);
    }

    private TeamData teamToTeamData(Team team) {
        return new TeamData(team.getId(),
                team.getOrganizationId(),
                team.getNumber(),
                team.getDivisionId());
    }

}
