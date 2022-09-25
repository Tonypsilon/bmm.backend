package de.tonypsilon.bmm.backend.organization.service;

import de.tonypsilon.bmm.backend.club.service.ClubService;
import de.tonypsilon.bmm.backend.organization.data.OrganizationRepository;
import de.tonypsilon.bmm.backend.season.service.SeasonService;
import org.springframework.stereotype.Service;

@Service
public class OrganizationService {

    private final OrganizationRepository organizationRepository;
    private final SeasonService seasonService;
    private final ClubService clubService;

    public OrganizationService(final OrganizationRepository organizationRepository,
                               SeasonService seasonService,
                               ClubService clubService) {
        this.organizationRepository = organizationRepository;
        this.seasonService = seasonService;
        this.clubService = clubService;
    }

    public Boolean existsByIdAndSeasonId(Long organizationId, Long seasonId) {
        return organizationRepository.existsByIdAndSeasonId(organizationId, seasonId);
    }

}
