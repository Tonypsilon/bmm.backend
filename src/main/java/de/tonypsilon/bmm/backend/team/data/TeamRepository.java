package de.tonypsilon.bmm.backend.team.data;

import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Collection;

public interface TeamRepository extends JpaRepository<Team, Long> {

    Team getByOrganizationIdAndNumber(Long organizationId, Integer number);

    Collection<Team> findByOrganizationId(Long organizationId);
}
