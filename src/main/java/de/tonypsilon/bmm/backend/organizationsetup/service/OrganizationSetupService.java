package de.tonypsilon.bmm.backend.organizationsetup.service;

import de.tonypsilon.bmm.backend.exception.BadDataException;
import de.tonypsilon.bmm.backend.exception.SeasonStageException;
import de.tonypsilon.bmm.backend.organization.data.OrganizationData;
import de.tonypsilon.bmm.backend.organization.service.OrganizationService;
import de.tonypsilon.bmm.backend.organizationsetup.data.TeamSetupData;
import de.tonypsilon.bmm.backend.participant.data.ParticipantCreationData;
import de.tonypsilon.bmm.backend.participant.data.ParticipantsCreationData;
import de.tonypsilon.bmm.backend.participant.service.ParticipantService;
import de.tonypsilon.bmm.backend.participationeligibility.service.ParticipationEligibilityService;
import de.tonypsilon.bmm.backend.season.service.SeasonService;
import de.tonypsilon.bmm.backend.season.service.SeasonStage;
import de.tonypsilon.bmm.backend.team.data.TeamCreationData;
import de.tonypsilon.bmm.backend.team.data.TeamData;
import de.tonypsilon.bmm.backend.team.service.TeamService;
import de.tonypsilon.bmm.backend.venue.data.VenueData;
import de.tonypsilon.bmm.backend.venue.service.VenueService;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Service;

import java.util.*;
import java.util.stream.Collectors;

import static java.util.function.Predicate.not;

@Service
public class OrganizationSetupService {

    private final TeamService teamService;
    private final OrganizationService organizationService;
    private final SeasonService seasonService;
    private final VenueService venueService;
    private final ParticipationEligibilityService participationEligibilityService;
    private final ParticipantService participantService;

    public OrganizationSetupService(final TeamService teamService,
                                    final OrganizationService organizationService,
                                    final SeasonService seasonService,
                                    final VenueService venueService,
                                    final ParticipationEligibilityService participationEligibilityService,
                                    final ParticipantService participantService) {
        this.teamService = teamService;
        this.organizationService = organizationService;
        this.seasonService = seasonService;
        this.venueService = venueService;
        this.participationEligibilityService = participationEligibilityService;
        this.participantService = participantService;
    }

    /**
     * Checks if the given setup of an organization is valid.
     * The given ParticipationEligibilities are ignored, they are only relevant for the get use case.
     * If yes, delete all current teams and their participants and creat a new configuration for the
     * organization.
     * If no, throw a suitable exception.
     *
     * @param organizationId the organization to create a setup for.
     * @param teamsSetupData the setup to be made.
     */
    public List<TeamSetupData> setUpTeamsOfOrganization(@NonNull Long organizationId,
                                         @NonNull List<TeamSetupData> teamsSetupData) {
        if (!teamsSetupData.stream()
                .map(TeamSetupData::organizationId)
                .allMatch(organizationId::equals)) {
            throw new BadDataException("Alle Teams müssen zur gegebenen Organisation gehören!");
        }
        if (containsDuplicateParticipants(teamsSetupData)) {
            throw new BadDataException("Es darf kein Spieler in mehreren Mannschaften vorkommen!");
        }
        OrganizationData organization = organizationService.getOrganizationById(organizationId);
        if(seasonService.getStageOfSeason(organization.seasonId()) != SeasonStage.REGISTRATION) {
            throw new SeasonStageException("Saison ist nicht in der Registrierungsphase!");
        }
        for (int i = 0; i < teamsSetupData.size(); i++) {
            if(!teamsSetupData.get(i).number().equals(i+1)) {
                throw new BadDataException("Die Teamnummern passen nicht zusammen oder sind nicht sortiert!");
            }
        }
        if(!teamsSetupData.stream()
                .map(TeamSetupData::participants)
                .flatMap(List::stream)
                .allMatch(participationEligibilityService
                        .isValidParticipationEligibilityForOrganization(organization))) {
            throw new BadDataException("Mindestens ein Spieler hat nicht die notwendige Spielberechtigung!");
        }
        if(!teamsSetupData.stream()
                .map(TeamSetupData::participants)
                .map(List::size)
                .allMatch(size -> size <= 16)) {
            throw new BadDataException("Kein Team darf mehr als 16 Spieler haben!");
        }
        if(!teamsSetupData.stream()
                .map(TeamSetupData::venueId)
                .allMatch(venueService.getVenuesForOrganization(organizationId).stream()
                        .map(VenueData::id)
                        .collect(Collectors.toSet())::contains)) {
            throw new BadDataException("Alle Mannschaften müssen einem Spielort der Organisation zugeordnet sein!");
        }

        participantService.deleteParticipantsOfOrganization(organizationId);
        teamService.deleteTeamsOfOrganization(organizationId);

        for (var teamSetupData : teamsSetupData) {
            TeamData createdTeam = teamService.createTeam(teamCreationDataFromTeamSetupData(teamSetupData));
            var participantCreationDataList = new ArrayList<ParticipantCreationData>();
            for(int i = 0; i < teamSetupData.participants().size(); ++i) {
                participantCreationDataList.add(new ParticipantCreationData(
                        createdTeam.id(), teamSetupData.participants().get(i), i+1));
            }
            participantService.createValidParticipantConfigurationForTeam(
                    new ParticipantsCreationData(createdTeam.id(), participantCreationDataList));
        }
        return teamsSetupData;
    }

    private TeamCreationData teamCreationDataFromTeamSetupData(TeamSetupData teamSetupData) {
        return new TeamCreationData(teamSetupData.organizationId(), teamSetupData.number(), teamSetupData.venueId());
    }

    private boolean containsDuplicateParticipants(Collection<TeamSetupData> teams) {
        Set<Long> duplicates = new HashSet<>();
        return teams.stream().map(TeamSetupData::participants)
                .flatMap(List::stream)
                .anyMatch(not(duplicates::add));
    }
}
