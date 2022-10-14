package de.tonypsilon.bmm.backend.organization.service;

import de.tonypsilon.bmm.backend.club.service.ClubService;
import de.tonypsilon.bmm.backend.exception.NotFoundException;
import de.tonypsilon.bmm.backend.organization.data.*;
import de.tonypsilon.bmm.backend.season.service.SeasonService;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

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
    public OrganizationData createOrganization(OrganizationCreationData organizationCreationData) {
        Organization organization = new Organization();
        organization.setSeasonId(organizationCreationData.seasonId());

        organizationRepository.save(organization);

        return toOrganizationData(
                organizationRepository.getBySeasonIdAndName(organizationCreationData.seasonId(),
                        organizationCreationData.name()));
    }

    public Long getSeasonIdOfOrganization(Long organizationId) {
        return organizationRepository.findById(organizationId)
                .map(Organization::getSeasonId)
                .orElseThrow(() -> new NotFoundException("Es gibt keine Organisation mit der ID %s!"
                        .formatted(organizationId)));
    }

    public Boolean existsByIdAndSeasonId(Long organizationId, Long seasonId) {
        return organizationRepository.existsByIdAndSeasonId(organizationId, seasonId);
    }

    private OrganizationData toOrganizationData(Organization organization) {
        return null;
    }

}
