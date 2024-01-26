package de.tonypsilon.bmm.backend.divisionsetup.service;

import de.tonypsilon.bmm.backend.division.service.DivisionService;
import de.tonypsilon.bmm.backend.exception.BadDataException;
import de.tonypsilon.bmm.backend.exception.SeasonStageException;
import de.tonypsilon.bmm.backend.season.service.SeasonService;
import de.tonypsilon.bmm.backend.season.service.SeasonStage;
import de.tonypsilon.bmm.backend.team.data.TeamDivisionLinkData;
import de.tonypsilon.bmm.backend.team.service.TeamDivisionLinkService;
import de.tonypsilon.bmm.backend.team.service.TeamService;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;

import static java.util.function.Predicate.not;
import static java.util.stream.Collectors.*;

@Service
public class DivisionSetupService {

    private final SeasonService seasonService;
    private final TeamDivisionLinkService teamDivisionLinkService;
    private final TeamService teamService;
    private final DivisionService divisionService;

    public DivisionSetupService(final SeasonService seasonService,
                                final TeamDivisionLinkService teamDivisionLinkService,
                                final TeamService teamService,
                                final DivisionService divisionService) {
        this.seasonService = seasonService;
        this.teamDivisionLinkService = teamDivisionLinkService;
        this.teamService = teamService;
        this.divisionService = divisionService;
    }

    @Transactional
    @NonNull
    public List<TeamDivisionLinkData> putTeamDivisionLinksForSeason(
            @NonNull Long seasonId, @NonNull List<TeamDivisionLinkData> teamDivisionLinks) {
        if(seasonService.getStageOfSeason(seasonId) != SeasonStage.PREPARATION) {
            throw new SeasonStageException("Saison ist nicht in der Vorbereitungsphase!");
        }
        if(!teamDivisionLinks.stream()
                .map(TeamDivisionLinkData::teamId)
                .map(teamService::getSeasonIdByTeamId)
                .allMatch(seasonId::equals)) {
            throw new BadDataException("Mindestens ein Team gehört nicht zur richtigen Saison!");
        }
        if(!teamDivisionLinks.stream()
                .map(TeamDivisionLinkData::divisionId)
                .map(divisionService::getSeasonIdByDivisionId)
                .allMatch(seasonId::equals)) {
            throw new BadDataException("Mindestens eine Staffel gehört nicht zur richtigen Saison!");
        }
        if(containsDuplicateTeams(teamDivisionLinks)) {
            throw new BadDataException("Jede Mannschaft muss genau ein Mal zugewiesen werden!");
        }
        if(containsDuplicatesForDivisionAndNumber(teamDivisionLinks)) {
            throw new BadDataException("Pro Staffel darf höchstens eine Mannschaft pro Platz gesetzt werden!");
        }
        teamDivisionLinkService.deleteBySeason(seasonId);
        return teamDivisionLinks.stream()
                .map(teamDivisionLinkService::createTeamDivisionLink)
                .toList();
    }

    private boolean containsDuplicateTeams(Collection<TeamDivisionLinkData> teamDivisionLinks) {
        Set<Long> uniqueTeamIds = new HashSet<>();
        return teamDivisionLinks.stream().map(TeamDivisionLinkData::teamId)
                .anyMatch(not(uniqueTeamIds::add));
    }

    private boolean containsDuplicatesForDivisionAndNumber(Collection<TeamDivisionLinkData> teamDivisionLinks) {
        return teamDivisionLinks.stream()
                .collect(groupingBy(
                        TeamDivisionLinkData::divisionId,
                        mapping(TeamDivisionLinkData::number, toList())))
                .values()
                .stream()
                .anyMatch(this::containsDuplicates);
    }

    private boolean containsDuplicates(List<Integer> numbers) {
        return numbers.size() > new HashSet<>(numbers).size();
    }
}
