package de.tonypsilon.bmm.backend.team.data;

import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Collection;

public interface TeamRepository extends JpaRepository<Team, Long> {

    public Boolean existsBySeasonIdAndClubIdAndNumber(Long seasonId, Long clubId, Integer number);

    public Team getBySeasonIdAndClubIdAndNumber(Long seasonId, Long clubId, Integer number);

    public Collection<Team> findBySeasonIdAndClubId(Long seasonId, Long clubId);
}
