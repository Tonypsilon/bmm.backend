package de.tonypsilon.bmm.backend.season.service;

import de.tonypsilon.bmm.backend.exception.BmmException;
import de.tonypsilon.bmm.backend.organization.service.OrganizationService;
import de.tonypsilon.bmm.backend.season.data.SeasonData;
import de.tonypsilon.bmm.backend.team.data.TeamData;
import de.tonypsilon.bmm.backend.team.service.TeamService;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

@Service
public class SeasonStageService {

    private final SeasonService seasonService;
    private final OrganizationService organizationService;
    private final TeamService teamService;


    public SeasonStageService(final SeasonService seasonService,
                              final OrganizationService organizationService,
                              final TeamService teamService) {
        this.seasonService = seasonService;
        this.organizationService = organizationService;
        this.teamService = teamService;
    }

    @Transactional
    public SeasonData changeSeasonStage(SeasonData seasonData, SeasonStage stage) {
        if (seasonData.stage() == SeasonStage.PREPARATION && stage == SeasonStage.RUNNING) {
            return changeSeasonStageFromPreparationToRunning(seasonData);
        }
        throw new BmmException("Fehler bei Saisonübergang von %s zu %s"
                .formatted(seasonData.stage(), stage));
    }

    private SeasonData changeSeasonStageFromRegistrationToPreparation(SeasonData seasonData) {
        Set<TeamData> allTeamsOfSeason = Stream.of(seasonData)
                .map(SeasonData::id)
                .map(organizationService::getOrganizationIdsOfSeason)
                .map(teamService::getTeamsByOrganizationIdIn)
                .flatMap(Set::stream)
                .collect(Collectors.toSet());
        verifyEveryTeamHasAtLeastEightPlayers(allTeamsOfSeason);
        verifyNoHolesInTeamNumberSequences(allTeamsOfSeason);
        verifyNoHolesInPlayerNumberSequences(seasonData.id());
        return new SeasonData(seasonData.id(), seasonData.name(), SeasonStage.PREPARATION);
    }

    private void verifyEveryTeamHasAtLeastEightPlayers(Set<TeamData> allTeamsOfSeason) {

    }

    private void verifyNoHolesInTeamNumberSequences(Set<TeamData> allTeamsOfSeason) {

    }

    private void verifyNoHolesInPlayerNumberSequences(Long seasonId) {

    }

    private SeasonData changeSeasonStageFromPreparationToRunning(SeasonData seasonData) {
        return seasonData;
    }
}
