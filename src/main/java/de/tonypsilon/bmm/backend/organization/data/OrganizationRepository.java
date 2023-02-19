package de.tonypsilon.bmm.backend.organization.data;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.Set;

@Repository
public interface OrganizationRepository extends JpaRepository<Organization, Long> {

    Organization getBySeasonIdAndName(Long seasonId, String name);

    Set<Organization> findBySeasonId(Long seasonId);
}
