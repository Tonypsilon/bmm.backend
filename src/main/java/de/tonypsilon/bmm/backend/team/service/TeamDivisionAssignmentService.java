package de.tonypsilon.bmm.backend.team.service;

import de.tonypsilon.bmm.backend.division.service.DivisionService;
import de.tonypsilon.bmm.backend.exception.BadDataException;
import de.tonypsilon.bmm.backend.exception.NotFoundException;
import de.tonypsilon.bmm.backend.exception.SeasonStageException;
import de.tonypsilon.bmm.backend.season.service.SeasonService;
import de.tonypsilon.bmm.backend.season.service.SeasonStage;
import de.tonypsilon.bmm.backend.team.data.TeamDivisionAssignment;
import de.tonypsilon.bmm.backend.team.data.TeamDivisionAssignmentData;
import de.tonypsilon.bmm.backend.team.data.TeamDivisionAssignmentRepository;
import org.springframework.lang.NonNull;
import org.springframework.lang.Nullable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
public class TeamDivisionAssignmentService {

    private final TeamDivisionAssignmentRepository teamDivisionAssignmentRepository;
    private final TeamService teamService;
    private final DivisionService divisionService;
    private final SeasonService seasonService;

    public TeamDivisionAssignmentService(final TeamDivisionAssignmentRepository teamDivisionAssignmentRepository,
                                         final TeamService teamService,
                                         final DivisionService divisionService,
                                         final SeasonService seasonService) {
        this.teamDivisionAssignmentRepository = teamDivisionAssignmentRepository;
        this.teamService = teamService;
        this.divisionService = divisionService;
        this.seasonService = seasonService;
    }

    @Transactional
    @NonNull
    public TeamDivisionAssignmentData createTeamDivisionAssignment(
            TeamDivisionAssignmentData creationData) {
        if (seasonService.getStageOfSeason(divisionService.getSeasonIdByDivisionId(creationData.divisionId()))
                != SeasonStage.PREPARATION) {
            throw new SeasonStageException("Saison ist nicht in der Vorbereitungsphase!");
        }
        if (teamDivisionAssignmentRepository.existsByDivisionIdAndNumber(
                creationData.divisionId(), creationData.number())) {
            throw new BadDataException(
                    "Es wurde für die Staffel %d bereits eine Mannschaft auf Platz %d gesetzt!"
                            .formatted(creationData.divisionId(), creationData.number()));
        }
        if (teamDivisionAssignmentRepository.existsByTeamId(creationData.teamId())) {
            throw new BadDataException("Die Mannschaft %d ist bereits einer Staffel zugeordnet!"
                    .formatted(creationData.teamId()));
        }
        if (creationData.number() < 1 || creationData.number() > 10) {
            throw new BadDataException("Die Setznummer muss eine Zahl zwischen 1 und 10 sein!");
        }
        if (!teamService.getSeasonIdByTeamId(creationData.teamId())
                .equals(divisionService.getSeasonIdByDivisionId(creationData.divisionId()))) {
            throw new BadDataException("Mannschaft %d und Staffel %d gehören nicht zur gleichen Saison!"
                    .formatted(creationData.teamId(), creationData.divisionId()));
        }
        TeamDivisionAssignment teamDivisionAssignment = new TeamDivisionAssignment();
        teamDivisionAssignment.setTeamId(creationData.teamId());
        teamDivisionAssignment.setDivisionId(creationData.divisionId());
        teamDivisionAssignment.setNumber(creationData.number());
        teamDivisionAssignmentRepository.save(teamDivisionAssignment);
        return teamDivisionAssignmentToDivisionAssignmentData(
                getByKey(creationData.teamId(), creationData.divisionId()));
    }

    @Nullable
    public Long getDivisionIdOfTeam(Long teamId) {
        return teamDivisionAssignmentRepository.findByTeamId(teamId)
                .map(TeamDivisionAssignment::getDivisionId)
                .orElse(null);
    }

    @NonNull
    private TeamDivisionAssignment getByKey(Long teamId, Long divisionId) {
        return teamDivisionAssignmentRepository.findByTeamIdAndDivisionId(teamId, divisionId)
                .orElseThrow(() -> new NotFoundException("Mannschaft %d ist nicht Staffel %d zugeordnet!"
                        .formatted(teamId, divisionId)));
    }

    @NonNull
    private TeamDivisionAssignmentData teamDivisionAssignmentToDivisionAssignmentData(
            @NonNull TeamDivisionAssignment teamDivisionAssignment) {
        return new TeamDivisionAssignmentData(teamDivisionAssignment.getTeamId(),
                teamDivisionAssignment.getDivisionId(),
                teamDivisionAssignment.getNumber());
    }

}
