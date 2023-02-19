package de.tonypsilon.bmm.backend.team.data;

import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Collection;
import java.util.Set;

public interface TeamRepository extends JpaRepository<Team, Long> {

    Team getByOrganizationIdAndNumber(Long organizationId, Integer number);

    Set<Team> findByOrganizationId(Long organizationId);

    Set<Team> findByOrganizationIdIn(Set<Long> organizationIds);
}
