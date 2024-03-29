package de.tonypsilon.bmm.backend.organization.service;

import de.tonypsilon.bmm.backend.club.service.ClubService;
import de.tonypsilon.bmm.backend.exception.*;
import de.tonypsilon.bmm.backend.organization.data.*;
import de.tonypsilon.bmm.backend.season.service.SeasonService;
import de.tonypsilon.bmm.backend.season.service.SeasonStage;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Collection;
import java.util.HashSet;
import java.util.Set;
import java.util.stream.Collectors;

@Service
public class OrganizationService {

    private final OrganizationRepository organizationRepository;
    private final OrganizationMemberRepository organizationMemberRepository;
    private final SeasonService seasonService;
    private final ClubService clubService;

    public OrganizationService(final OrganizationRepository organizationRepository,
                               final OrganizationMemberRepository organizationMemberRepository,
                               final SeasonService seasonService,
                               final ClubService clubService) {
        this.organizationRepository = organizationRepository;
        this.organizationMemberRepository = organizationMemberRepository;
        this.seasonService = seasonService;
        this.clubService = clubService;
    }

    @Transactional
    @NonNull
    public OrganizationData createOrganization(OrganizationCreationData organizationCreationData) {
        if(organizationCreationData.name() == null || organizationCreationData.name().isEmpty()) {
            throw new BadDataException("Der Name der Organisation darf nicht leer sein!");
        }
        if(!seasonService.seasonExistsById(organizationCreationData.seasonId())) {
            throw new NotFoundException(
                    "Es gibt keine Saison mit der ID %d!".formatted(organizationCreationData.seasonId()));
        }
        if(seasonService.getStageOfSeason(organizationCreationData.seasonId()) != SeasonStage.REGISTRATION) {
            throw new SeasonStageException("Saison ist nicht in der Registrierungsphase!");
        }
        if(organizationCreationData.clubIds() == null || organizationCreationData.clubIds().isEmpty()) {
            throw new BadDataException("Zur Erstellung einer Organisation muss mindestens ein Verein gegeben sein!");
        }
        validateClubIds(organizationCreationData.clubIds(), organizationCreationData.seasonId());
        if(organizationCreationData.firstTeamNumber() == null || organizationCreationData.firstTeamNumber() < 1) {
            throw new BadDataException("Die erste Mannschaftsnummer muss eine ganze Zahl größer 0 sein!");
        }

        Organization organization = new Organization();
        organization.setSeasonId(organizationCreationData.seasonId());
        organization.setName(organizationCreationData.name());
        organization.setFirstTeamNumber(organizationCreationData.firstTeamNumber());

        organization.setOrganizationMembers(new HashSet<>());
        organizationCreationData.clubIds()
                .forEach(clubId ->
                    organization.getOrganizationMembers().add(createOrganizationMember(clubId, organization))
                );

        organizationRepository.save(organization);
        return toOrganizationData(
                organizationRepository.getBySeasonIdAndName(organizationCreationData.seasonId(),
                        organizationCreationData.name()));
    }

    // checks if clubs exist and also ensures they are not yet part of another organization for that season.
    private void validateClubIds(Collection<Long> clubIds, Long seasonId) {
        clubIds.forEach(clubService::verifyClubExistsById);
        Set<Long> organizationIdsOfCurrentSeason = getOrganizationIdsOfSeason(seasonId);
        organizationMemberRepository.findByOrganizationIdIn(organizationIdsOfCurrentSeason)
                .stream()
                .map(OrganizationMember::getClubId)
                .filter(clubIds::contains)
                .findFirst()
                .ifPresent(id -> {
                    throw new AlreadyExistsException(
                            "Es gibt schon eine Organisation in der Saison mit der ID %d für den Verein mit der ID %d!"
                                    .formatted(seasonId, id));
                });
    }

    private OrganizationMember createOrganizationMember(Long clubId, Organization organization) {
        OrganizationMember organizationMember = new OrganizationMember();
        organizationMember.setOrganization(organization);
        organizationMember.setClubId(clubId);
        return organizationMember;
    }

    @NonNull
    public Set<Long> getOrganizationIdsOfSeason(@NonNull Long seasonId) {
        return organizationRepository.findBySeasonId(seasonId)
                .stream()
                .map(Organization::getId)
                .collect(Collectors.toSet());
    }

    @Transactional
    @NonNull
    public Long getSeasonIdOfOrganization(@NonNull Long organizationId) {
        return getById(organizationId).getSeasonId();
    }

    @NonNull
    public OrganizationData getOrganizationById(@NonNull Long organizationId) {
        return toOrganizationData(getById(organizationId));
    }

    @NonNull
    public Set<OrganizationData> getAll() {
        return organizationRepository.findAll().stream()
                .map(this::toOrganizationData)
                .collect(Collectors.toSet());
    }

    private Organization getById(Long organizationId) {
        return organizationRepository.findById(organizationId).orElseThrow(
                () -> new NotFoundException("Es gibt keine Organisation mit der ID %d!"
                        .formatted(organizationId)));
    }

    private OrganizationData toOrganizationData(Organization organization) {
        return new OrganizationData(organization.getId(),
                organization.getSeasonId(),
                organization.getName(),
                organization.getFirstTeamNumber(),
                organization.getOrganizationMembers()
                        .stream().map(OrganizationMember::getClubId).collect(Collectors.toSet()));
    }

}
