package de.tonypsilon.bmm.backend.team.service;

import de.tonypsilon.bmm.backend.exception.BadDataException;
import de.tonypsilon.bmm.backend.exception.NotFoundException;
import de.tonypsilon.bmm.backend.exception.SeasonStageException;
import de.tonypsilon.bmm.backend.organization.service.OrganizationService;
import de.tonypsilon.bmm.backend.season.service.SeasonService;
import de.tonypsilon.bmm.backend.season.service.SeasonStage;
import de.tonypsilon.bmm.backend.security.rnr.Role;
import de.tonypsilon.bmm.backend.security.rnr.data.User;
import de.tonypsilon.bmm.backend.security.rnr.data.UserData;
import de.tonypsilon.bmm.backend.security.rnr.service.UserService;
import de.tonypsilon.bmm.backend.team.data.*;
import de.tonypsilon.bmm.backend.venue.service.VenueService;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

@Service
public class TeamService {

    private final TeamRepository teamRepository;
    private final SeasonService seasonService;
    private final OrganizationService organizationService;
    private final VenueService venueService;
    private final UserService userService;

    public TeamService(final TeamRepository teamRepository,
                       final SeasonService seasonService,
                       final OrganizationService organizationService,
                       final VenueService venueService,
                       final UserService userService) {
        this.teamRepository = teamRepository;
        this.seasonService = seasonService;
        this.organizationService = organizationService;
        this.venueService = venueService;
        this.userService = userService;
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

        if(!organizationService.getOrganizationById(teamCreationData.organizationId()).clubIds()
                .contains(venueService.getClubIdByVenueId(teamCreationData.venueId()))) {
            throw new BadDataException("Das Spiellokal gehört zu keinem Verein der Organisation!");
        }

        verifyTeamNumber(teamCreationData);

        UserData userData = userService.getUserDataByUsername(teamCreationData.captainUsername());
        if(!userData.roles().contains(Role.TEAM_ADMIN)) {
            userService.assignRoleToUser(userData.username(), Role.TEAM_ADMIN);
        }

        if(teamCreationData.name() != null && teamCreationData.name().length() > 64) {
            throw new BadDataException("Der Name des Teams ist zu lang, maximal 64 Zeichen!");
        }

        Team team = new Team();
        team.setOrganizationId(teamCreationData.organizationId());
        team.setNumber(teamCreationData.number());
        team.setVenueId(teamCreationData.venueId());
        team.setName(teamCreationData.name());
        team.setCaptainUsername(teamCreationData.captainUsername());

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

    @Transactional
    public void deleteTeamsOfOrganization(Long organizationId) {
        if(seasonService.getStageOfSeason(organizationService.getSeasonIdOfOrganization(organizationId))
                != SeasonStage.REGISTRATION) {
            throw new SeasonStageException("Saison ist nicht in der Registrierungsphase!");
        }
        teamRepository.deleteAll(teamRepository.findByOrganizationId(organizationId));
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
    public Set<TeamData> getTeamsByOrganizationIdIn(Set<Long> organizationIds) {
        return teamRepository.findByOrganizationIdIn(organizationIds).stream()
                .map(this::teamToTeamData)
                .collect(Collectors.toSet());
    }

    @NonNull
    public List<TeamData> getTeamsOfOrganization(@NonNull Long organizationId) {
        return teamRepository.findByOrganizationId(organizationId).stream()
                .map(this::teamToTeamData)
                .toList();
    }

    @NonNull
    private Team getById(@NonNull Long teamId) {
        return teamRepository.findById(teamId).orElseThrow(
                () -> new NotFoundException("Es gibt keine Mannschaft mit der ID %d".formatted(teamId)));
    }

    @NonNull
    public Long getSeasonIdByTeamId(@NonNull Long teamId) {
        return organizationService.getSeasonIdOfOrganization(getById(teamId).getOrganizationId());
    }

    @NonNull
    public String getNameOfTeam(@NonNull Long teamId) {
        Team team = getById(teamId);
        return organizationService.getOrganizationById(team.getOrganizationId()).name() + " " + team.getNumber();
    }

    private void verifyTeamNumber(TeamCreationData teamCreationData) {
        int maxTeamNumber = getMaxTeamNumberForTeamsOfOrganization(teamCreationData.organizationId());
        if (!teamCreationData.number().equals(maxTeamNumber+1)) {
            throw new BadDataException(
                    "Das neue Team hat nicht die passende Teamnummer. Erwartet: %d. Tatsächlich: %d."
                            .formatted(maxTeamNumber+1, teamCreationData.number()));
        }
    }

    private Integer getMaxTeamNumberForTeamsOfOrganization(Long organizationId) {
        return  teamRepository.findByOrganizationId(organizationId)
                .stream()
                .map(Team::getNumber)
                .max(Integer::compareTo)
                .orElse(0);
    }

    private TeamData teamToTeamData(Team team) {
        return new TeamData(team.getId(),
                team.getOrganizationId(),
                team.getNumber(),
                team.getVenueId(),
                team.getName(),
                team.getCaptainUsername());
    }

}
