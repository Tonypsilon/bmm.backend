package de.tonypsilon.bmm.backend.standings.service;

import de.tonypsilon.bmm.backend.datatypes.IdAndLabel;
import de.tonypsilon.bmm.backend.division.data.DivisionData;
import de.tonypsilon.bmm.backend.division.service.DivisionService;
import de.tonypsilon.bmm.backend.match.data.ParticipantDataForClient;
import de.tonypsilon.bmm.backend.participant.data.ParticipantData;
import de.tonypsilon.bmm.backend.participant.service.ParticipantService;
import de.tonypsilon.bmm.backend.participationeligibility.data.ParticipationEligibilityData;
import de.tonypsilon.bmm.backend.participationeligibility.service.ParticipationEligibilityService;
import de.tonypsilon.bmm.backend.standings.data.ParticipantProgressChartData;
import de.tonypsilon.bmm.backend.standings.data.ProgressChartData;
import de.tonypsilon.bmm.backend.standings.data.TeamProgressChartData;
import de.tonypsilon.bmm.backend.team.data.TeamData;
import de.tonypsilon.bmm.backend.team.data.TeamDivisionLinkData;
import de.tonypsilon.bmm.backend.team.service.TeamDivisionLinkService;
import de.tonypsilon.bmm.backend.team.service.TeamService;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Service;

import java.util.Comparator;
import java.util.List;

@Service
public class ProgressChartAssembler {

    private final DivisionService divisionService;
    private final TeamDivisionLinkService teamDivisionLinkService;
    private final TeamService teamService;
    private final ParticipantService participantService;
    private final ParticipationEligibilityService participationEligibilityService;

    public ProgressChartAssembler(final DivisionService divisionService,
                                  final TeamDivisionLinkService teamDivisionLinkService,
                                  final TeamService teamService,
                                  final ParticipantService participantService,
                                  final ParticipationEligibilityService participationEligibilityService) {
        this.divisionService = divisionService;
        this.teamDivisionLinkService = teamDivisionLinkService;
        this.teamService = teamService;
        this.participantService = participantService;
        this.participationEligibilityService = participationEligibilityService;
    }

    @NonNull
    public ProgressChartData assembleProgressChart(@NonNull Long divisionId) {
        DivisionData divisionData = divisionService.getDivisionDataById(divisionId);
        List<TeamData> teams = teamDivisionLinkService.getByDivisionId(divisionId).stream()
                .sorted(Comparator.comparingInt(TeamDivisionLinkData::number))
                .map(TeamDivisionLinkData::teamId)
                .map(teamService::getTeamDataById)
                .toList();

        return new ProgressChartData(
                divisionData.numberOfTeams()-1,
                teams.stream()
                        .map(teamData -> assembleProgressChartDataForTeam(teamData, divisionId))
                        .toList()
        );
    }

    private TeamProgressChartData assembleProgressChartDataForTeam(@NonNull TeamData teamData,
                                                                   @NonNull Long divisionId) {
        List<ParticipantData> participantsOfTeam =
                participantService.getParticipantsOfTeamOrderedByNumberAsc(teamData.id());
        return new TeamProgressChartData(
                new IdAndLabel(teamData.id(), teamData.name()),
                participantsOfTeam.stream()
                        .map(participantData -> this.assembleParticipantProgressChartData(participantData, divisionId))
                        .toList()
        );
    }

    @NonNull
    private ParticipantProgressChartData assembleParticipantProgressChartData(
            @NonNull ParticipantData participantData,
            @NonNull Long divisionId) {
        ParticipationEligibilityData participationEligibilityData =
                participationEligibilityService.getParticipationEligibilityById(
                        participantData.participationEligibilityId());
        return new ParticipantProgressChartData(
                new ParticipantDataForClient(participantData.id(),
                        participantService.getCodeOfParticipant(participantData.id()),
                        participationEligibilityData.forename(),
                        participationEligibilityData.surname(),
                        participationEligibilityData.dwz()),
                null
        );
    }
}
