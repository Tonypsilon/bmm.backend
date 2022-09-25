package de.tonypsilon.bmm.backend.organization.data;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface OrganizationRepository extends JpaRepository<Organization, Long> {

    public Boolean existsByIdAndSeasonId(Long id, Long seasonId);
}
