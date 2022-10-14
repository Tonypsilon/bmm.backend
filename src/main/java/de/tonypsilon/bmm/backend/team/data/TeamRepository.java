package de.tonypsilon.bmm.backend.team.data;

import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Collection;

public interface TeamRepository extends JpaRepository<Team, Long> {

    public Team getByOrganizationIdAndNumber(Long organizationId, Integer number);

    public Collection<Team> findByOrganizationId(Long organizationId);
}
