package de.tonypsilon.bmm.backend.team.data;

import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Collection;

public interface TeamRepository extends JpaRepository<Team, Long> {

    public Team getBySeasonIdAndOrganizationIdAndNumber(Long seasonId, Long organizationId, Integer number);

    public Collection<Team> findBySeasonIdAndOrganizationId(Long seasonId, Long organizationId);
}
