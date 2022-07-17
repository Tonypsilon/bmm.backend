package de.tonypsilon.bmm.backend.team.service;

import de.tonypsilon.bmm.backend.club.service.ClubService;
import de.tonypsilon.bmm.backend.exception.BadDataException;
import de.tonypsilon.bmm.backend.exception.NotFoundException;
import de.tonypsilon.bmm.backend.exception.SeasonStageException;
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
    private final ClubService clubService;

    public TeamService(final TeamRepository teamRepository,
                       SeasonService seasonService,
                       ClubService clubService) {
        this.teamRepository = teamRepository;
        this.seasonService = seasonService;
        this.clubService = clubService;
    }

    @Transactional
    public TeamData createTeam(TeamCreationData teamCreationData) {
        if(!seasonService.seasonExistsById(teamCreationData.seasonId())) {
            throw new NotFoundException("Es gibt keine Saison mit ID %d!".formatted(teamCreationData.seasonId()));
        }
        if (!seasonService.getStageOfSeason(teamCreationData.seasonId()).equals(SeasonStage.REGISTRATION)) {
            throw new SeasonStageException("Saison ist nicht in der Registrierungsphase!");
        }
        if(!clubService.clubExistsById(teamCreationData.clubId())) {
            throw new NotFoundException("Es gibt keinen Verein mit ID %d!".formatted(teamCreationData.clubId()));
        }
        verifyTeamNumber(teamCreationData);

        Team team = new Team();
        team.setSeasonId(teamCreationData.seasonId());
        team.setClubId(teamCreationData.clubId());
        team.setNumber(teamCreationData.number());

        teamRepository.save(team);

        return teamToTeamData(teamRepository.getBySeasonIdAndClubIdAndNumber(teamCreationData.seasonId(),
                teamCreationData.clubId(),
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
        if(!team.getNumber().equals(getMaxTeamNumberForTeamsOfClub(team.getSeasonId(), team.getClubId()))) {
            throw new BadDataException(
                    "Es kann nur das Team mit der höchsten Nummer gelöscht werden!"
            );
        }
        teamRepository.delete(team);
    }

    public Boolean existsById(Long teamId) {
        return teamRepository.existsById(teamId);
    }

    private void verifyTeamNumber(TeamCreationData teamCreationData) {
        Integer maxTeamNumber = getMaxTeamNumberForTeamsOfClub(teamCreationData.seasonId(), teamCreationData.clubId());
        if (!teamCreationData.number().equals(maxTeamNumber+1)) {
            throw new BadDataException(
                    "Das neue Team hat nicht die passende Teamnummer. Erwartet: %d. Tatsächlich: %d."
                            .formatted(maxTeamNumber+1, teamCreationData.number()));
        }
    }

    private Integer getMaxTeamNumberForTeamsOfClub(Long seasonId, Long clubId) {
        List<Integer> teamNumbers = teamRepository.findBySeasonIdAndClubId(
                        seasonId, clubId)
                .stream()
                .map(Team::getNumber)
                .toList();
        // TODO Log if team number sequence is not good.
        return teamNumbers.stream().max(Integer::compareTo).orElse(0);
    }

    private TeamData teamToTeamData(Team team) {
        return new TeamData(team.getId(),
                team.getSeasonId(),
                team.getClubId(),
                team.getNumber());
    }
}
