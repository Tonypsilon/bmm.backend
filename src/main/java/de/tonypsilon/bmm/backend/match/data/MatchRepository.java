package de.tonypsilon.bmm.backend.match.data;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.Set;

@Repository
public interface MatchRepository extends JpaRepository<Match, Long> {

    public Set<Match> findByMatchdayId(Long matchdayId);

    public Boolean existsByHomeTeamIdOrAwayTeamIdEquals(Long teamId);

    public Boolean existsByMatchdayIdAndHomeTeamIdAndAwayTeamId(Long matchdayId, Long homeTeamId, Long awayTeamId);

    public Match getByMatchdayIdAndHomeTeamIdAndAwayTeamId(Long matchdayId, Long homeTeamId, Long awayTeamId);
}
