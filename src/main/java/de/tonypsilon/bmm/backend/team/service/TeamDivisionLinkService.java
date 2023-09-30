package de.tonypsilon.bmm.backend.team.service;

import de.tonypsilon.bmm.backend.division.data.DivisionData;
import de.tonypsilon.bmm.backend.division.service.DivisionService;
import de.tonypsilon.bmm.backend.exception.BadDataException;
import de.tonypsilon.bmm.backend.exception.NotFoundException;
import de.tonypsilon.bmm.backend.exception.SeasonStageException;
import de.tonypsilon.bmm.backend.season.service.SeasonService;
import de.tonypsilon.bmm.backend.season.service.SeasonStage;
import de.tonypsilon.bmm.backend.team.data.TeamDivisionLink;
import de.tonypsilon.bmm.backend.team.data.TeamDivisionLinkData;
import de.tonypsilon.bmm.backend.team.data.TeamDivisionLinkRepository;
import org.springframework.lang.NonNull;
import org.springframework.lang.Nullable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Set;
import java.util.stream.Collectors;

@Service
public class TeamDivisionLinkService {

    private final TeamDivisionLinkRepository teamDivisionLinkRepository;
    private final TeamService teamService;
    private final DivisionService divisionService;
    private final SeasonService seasonService;

    public TeamDivisionLinkService(final TeamDivisionLinkRepository teamDivisionLinkRepository,
                                   final TeamService teamService,
                                   final DivisionService divisionService,
                                   final SeasonService seasonService) {
        this.teamDivisionLinkRepository = teamDivisionLinkRepository;
        this.teamService = teamService;
        this.divisionService = divisionService;
        this.seasonService = seasonService;
    }

    @Transactional
    @NonNull
    public TeamDivisionLinkData createTeamDivisionLink(
            TeamDivisionLinkData creationData) {
        if (seasonService.getStageOfSeason(divisionService.getSeasonIdByDivisionId(creationData.divisionId()))
                != SeasonStage.PREPARATION) {
            throw new SeasonStageException("Saison ist nicht in der Vorbereitungsphase!");
        }
        if (teamDivisionLinkRepository.existsByDivisionIdAndNumber(
                creationData.divisionId(), creationData.number())) {
            throw new BadDataException(
                    "Es wurde für die Staffel %d bereits eine Mannschaft auf Platz %d gesetzt!"
                            .formatted(creationData.divisionId(), creationData.number()));
        }
        if (teamDivisionLinkRepository.existsByTeamId(creationData.teamId())) {
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
        TeamDivisionLink teamDivisionLink = new TeamDivisionLink();
        teamDivisionLink.setTeamId(creationData.teamId());
        teamDivisionLink.setDivisionId(creationData.divisionId());
        teamDivisionLink.setNumber(creationData.number());
        teamDivisionLinkRepository.save(teamDivisionLink);
        return teamDivisionLinkToTeamDivisionLinkData(
                getByKey(creationData.teamId(), creationData.divisionId()));
    }

    @Nullable
    public Long getDivisionIdOfTeam(Long teamId) {
        return teamDivisionLinkRepository.findByTeamId(teamId)
                .map(TeamDivisionLink::getDivisionId)
                .orElse(null);
    }

    @NonNull
    public Set<TeamDivisionLinkData> getByDivisionId(Long divisionId) {
        return teamDivisionLinkRepository.findByDivisionId(divisionId).stream()
                .map(this::teamDivisionLinkToTeamDivisionLinkData)
                .collect(Collectors.toSet());
    }

    @NonNull
    public Set<TeamDivisionLinkData> getBySeason(Long seasonId) {
        return divisionService.getAllDivisionsOfSeason(seasonId)
                .stream()
                .map(DivisionData::id)
                .map(this::getByDivisionId)
                .flatMap(Set::stream)
                .collect(Collectors.toSet());
    }

    @NonNull
    private TeamDivisionLink getByKey(Long teamId, Long divisionId) {
        return teamDivisionLinkRepository.findByTeamIdAndDivisionId(teamId, divisionId)
                .orElseThrow(() -> new NotFoundException("Mannschaft %d ist nicht Staffel %d zugeordnet!"
                        .formatted(teamId, divisionId)));
    }

    @NonNull
    private TeamDivisionLinkData teamDivisionLinkToTeamDivisionLinkData(
            @NonNull TeamDivisionLink teamDivisionLink) {
        return new TeamDivisionLinkData(teamDivisionLink.getTeamId(),
                teamDivisionLink.getDivisionId(),
                teamDivisionLink.getNumber());
    }

}
