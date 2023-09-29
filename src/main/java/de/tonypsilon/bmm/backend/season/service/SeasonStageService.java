package de.tonypsilon.bmm.backend.season.service;

import de.tonypsilon.bmm.backend.exception.BmmException;
import de.tonypsilon.bmm.backend.organization.service.OrganizationService;
import de.tonypsilon.bmm.backend.participant.data.ParticipantData;
import de.tonypsilon.bmm.backend.participant.service.ParticipantService;
import de.tonypsilon.bmm.backend.season.data.SeasonData;
import de.tonypsilon.bmm.backend.season.data.SeasonStageChangeData;
import de.tonypsilon.bmm.backend.team.data.TeamData;
import de.tonypsilon.bmm.backend.team.service.TeamDivisionLinkService;
import de.tonypsilon.bmm.backend.team.service.TeamService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

@Service
public class SeasonStageService {

    private final Logger logger = LoggerFactory.getLogger(SeasonStageService.class);

    private final SeasonService seasonService;
    private final OrganizationService organizationService;
    private final TeamService teamService;
    private final ParticipantService participantService;
    private final TeamDivisionLinkService teamDivisionLinkService;
    private final SeasonStartService seasonStartService;

    public SeasonStageService(final SeasonService seasonService,
                              final OrganizationService organizationService,
                              final TeamService teamService,
                              final ParticipantService participantService,
                              final TeamDivisionLinkService teamDivisionLinkService,
                              final SeasonStartService seasonStartService) {
        this.seasonService = seasonService;
        this.organizationService = organizationService;
        this.teamService = teamService;
        this.participantService = participantService;
        this.teamDivisionLinkService = teamDivisionLinkService;
        this.seasonStartService = seasonStartService;
    }

    @Transactional
    public SeasonData changeSeasonStage(SeasonStageChangeData seasonStageChangeData) {
        SeasonData seasonData = seasonService.getSeasonByName(seasonStageChangeData.seasonName());
        if(seasonData.stage() == seasonStageChangeData.stage()) {
            throw new BmmException("Saison %s ist schon in Phase %s".formatted(seasonData.name(), seasonData.stage()));
        }
        if(seasonData.stage() == SeasonStage.REGISTRATION && seasonStageChangeData.stage() == SeasonStage.PREPARATION) {
            verifyChangeSeasonStageFromRegistrationToPreparation(seasonData);
            return seasonService.updateSeasonStage(seasonStageChangeData);
        }
        if(seasonData.stage() == SeasonStage.PREPARATION && seasonStageChangeData.stage() == SeasonStage.RUNNING) {
            verifyChangeSeasonStageFromPreparationToRunning(seasonData);
            prepareSeasonStart(seasonData);
            return seasonService.updateSeasonStage(seasonStageChangeData);
        }
        return seasonService.updateSeasonStage(seasonStageChangeData);
    }

    private void verifyChangeSeasonStageFromRegistrationToPreparation(SeasonData seasonData) {
        Set<TeamData> allTeamsOfSeason = getAllTeamsOfSeason(seasonData);
        verifyPlayersOfTeams(allTeamsOfSeason);
        verifyNoHolesInTeamNumberSequences(allTeamsOfSeason);
    }

    // Verify each team has at least 8 players and that there are no holes in the
    // player number sequence.
    private void verifyPlayersOfTeams(Set<TeamData> allTeamsOfSeason) {
        for(TeamData teamData : allTeamsOfSeason) {
            List<ParticipantData> participants = participantService.getParticipantsOfTeamOrderedByNumberAsc(teamData.id());
            if (participants.size() < 8 ) {
                logger.error("Mannschaft mit ID %d hat weniger als 8 Spieler!".formatted(teamData.id()));
            }
            for(int i = 0; i < participants.size(); i++) {
                if (!participants.get(i).number().equals(i+1)) {
                    logger.error("Die Spielernummern der Mannschaft mit ID %d haben eine Lücke!".formatted(teamData.id()));
                }
            }
        }
    }

    private void verifyNoHolesInTeamNumberSequences(Set<TeamData> allTeamsOfSeason) {
        Map<Long, List<TeamData>> teamsByOrganization = allTeamsOfSeason.stream()
                .collect(Collectors.groupingBy(TeamData::organizationId));
        for (List<TeamData> teams : teamsByOrganization.values()) {
            teams.sort(Comparator.comparing(TeamData::number));
            for (int i = 0; i < teams.size(); i++) {
                if(!teams.get(i).number().equals(i+1)) {
                    logger.error("Organization mit ID %d hat eine Lücke in den Mannschaftsnummern!"
                            .formatted(teams.get(0).organizationId())); // teams can not be empty here, no danger of NPE
                }
            }
        }
    }

    private void verifyChangeSeasonStageFromPreparationToRunning(SeasonData seasonData) {
        verifyAllTeamsAreLinkedToDivision(getAllTeamsOfSeason(seasonData));
    }

    private void verifyAllTeamsAreLinkedToDivision(Set<TeamData> teams) {
        List<Long> teamIdsWithoutDivision = teams.stream()
                .map(TeamData::id)
                .filter(id -> teamDivisionLinkService.getDivisionIdOfTeam(id) == null)
                .toList();
        if (!teamIdsWithoutDivision.isEmpty()) {
            throw new BmmException("Es gibt mindestens eine Mannschaft ohne Staffel: "
                    + teamIdsWithoutDivision);
        }
    }

    private void prepareSeasonStart(@NonNull SeasonData seasonData) {
        seasonStartService.createMatchesForSeason(seasonData);
    }

    private Set<TeamData> getAllTeamsOfSeason(SeasonData seasonData) {
        return Stream.of(seasonData)
                .map(SeasonData::id)
                .map(organizationService::getOrganizationIdsOfSeason)
                .map(teamService::getTeamsByOrganizationIdIn)
                .flatMap(Set::stream)
                .collect(Collectors.toSet());
    }
}
