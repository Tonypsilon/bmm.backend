package de.tonypsilon.bmm.backend.organizationsetup.service;

import de.tonypsilon.bmm.backend.exception.BadDataException;
import de.tonypsilon.bmm.backend.exception.SeasonStageException;
import de.tonypsilon.bmm.backend.organization.data.OrganizationData;
import de.tonypsilon.bmm.backend.organization.service.OrganizationService;
import de.tonypsilon.bmm.backend.organizationsetup.data.OrganizationSetupData;
import de.tonypsilon.bmm.backend.organizationsetup.data.TeamSetupData;
import de.tonypsilon.bmm.backend.season.service.SeasonService;
import de.tonypsilon.bmm.backend.season.service.SeasonStage;
import de.tonypsilon.bmm.backend.team.service.TeamService;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Service;

import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import static java.util.function.Predicate.not;

@Service
public class OrganizationSetupService {

    private final TeamService teamService;
    private final OrganizationService organizationService;
    private final SeasonService seasonService;

    public OrganizationSetupService(final TeamService teamService,
                                    final OrganizationService organizationService,
                                    final SeasonService seasonService) {
        this.teamService = teamService;
        this.organizationService = organizationService;
        this.seasonService = seasonService;
    }

    /**
     * Checks if the given setup of an organization is valid.
     * The given ParticipationEligibilities are ignored, they are only relevant for the return value.
     * If yes, delete all current teams and their participants and creat a new configuration for the
     * organization.
     * If no, throw a suitable exception.
     *
     * @param organizationId the organization to create a setup for.
     * @param organizationSetupData the setup to be made.
     */
    public OrganizationSetupData setUpTeamsOfOrganization(@NonNull Long organizationId,
                                         @NonNull OrganizationSetupData organizationSetupData) {
        // 1. Check if the given configuration is valid.
        //    The given ParticipationEligibilities are ignored, only relevant for return value.
        // 2a. If yes, delete all current teams and their participants and create a new config.
        // 2b. If no, throw a suitable exception.
        if (!organizationSetupData.teams().stream()
                .map(TeamSetupData::organizationId)
                .allMatch(organizationId::equals)) {
            throw new BadDataException("Alle Teams müssen zur gegebenen Organisation gehören!");
        }
        if (containsDuplicates(organizationSetupData.teams())) {
            throw new BadDataException("Es darf kein Spieler in mehreren Mannschaften vorkommen!");
        }
        OrganizationData organization = organizationService.getOrganizationById(organizationId);
        if(seasonService.getStageOfSeason(organization.seasonId()) != SeasonStage.REGISTRATION) {
            throw new SeasonStageException("Saison ist nicht in der Vorbereitungsphase!");
        }
        return organizationSetupData;
    }

    private boolean containsDuplicates(Collection<TeamSetupData> teams) {
        Set<Long> duplicates = new HashSet<>();
        return teams.stream().map(TeamSetupData::participants)
                .flatMap(List::stream)
                .anyMatch(not(duplicates::add));
    }
}
