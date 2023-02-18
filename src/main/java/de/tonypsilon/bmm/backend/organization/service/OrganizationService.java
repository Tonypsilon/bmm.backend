package de.tonypsilon.bmm.backend.organization.service;

import de.tonypsilon.bmm.backend.club.service.ClubService;
import de.tonypsilon.bmm.backend.exception.*;
import de.tonypsilon.bmm.backend.organization.data.*;
import de.tonypsilon.bmm.backend.season.service.SeasonService;
import de.tonypsilon.bmm.backend.season.service.SeasonStage;
import de.tonypsilon.bmm.backend.security.rnr.service.ClubAdminService;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Collection;
import java.util.HashSet;
import java.util.Set;
import java.util.stream.Collectors;

import static java.util.function.Predicate.not;

@Service
public class OrganizationService {

    private final OrganizationRepository organizationRepository;
    private final OrganizationMemberRepository organizationMemberRepository;
    private final SeasonService seasonService;
    private final ClubService clubService;
    private final ClubAdminService clubAdminService;

    public OrganizationService(final OrganizationRepository organizationRepository,
                               final OrganizationMemberRepository organizationMemberRepository,
                               final SeasonService seasonService,
                               final ClubService clubService,
                               final ClubAdminService clubAdminService) {
        this.organizationRepository = organizationRepository;
        this.organizationMemberRepository = organizationMemberRepository;
        this.seasonService = seasonService;
        this.clubService = clubService;
        this.clubAdminService = clubAdminService;
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

        Organization organization = new Organization();
        organization.setSeasonId(organizationCreationData.seasonId());
        organization.setName(organizationCreationData.name());

        organization.setOrganizationMembers(new HashSet<>());
        organizationCreationData.clubIds()
                .forEach(clubId -> {
                    organization.getOrganizationMembers().add(createOrganizationMember(clubId, organization));
                });

        organizationRepository.save(organization);
        return toOrganizationData(
                organizationRepository.getBySeasonIdAndName(organizationCreationData.seasonId(),
                        organizationCreationData.name()));
    }

    // checks if clubs exist and also ensures they are not yet part of another organization for that season.
    private void validateClubIds(Collection<Long> clubIds, Long seasonId) {
        clubIds.stream().forEach(clubService::verifyClubExistsById);
        Set<Long> organizationIdsOfCurrentSeason = organizationRepository.findBySeasonId(seasonId)
                .stream()
                .map(Organization::getId)
                .collect(Collectors.toSet());
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

    @Transactional
    @NonNull
    public Long getSeasonIdOfOrganization(@NonNull Long organizationId) {
        return getById(organizationId).getSeasonId();
    }

    @NonNull
    public OrganizationData getOrganizationById(@NonNull Long organizationId) {
        return toOrganizationData(getById(organizationId));
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
                organization.getOrganizationMembers()
                        .stream().map(OrganizationMember::getClubId).collect(Collectors.toSet()));
    }

}
