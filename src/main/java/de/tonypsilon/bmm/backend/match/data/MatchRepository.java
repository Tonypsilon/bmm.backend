package de.tonypsilon.bmm.backend.match.data;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.util.Set;

@Repository
public interface MatchRepository extends JpaRepository<Match, Long> {

    public Set<Match> findByMatchdayId(Long matchdayId);

    @Query(value = "select m from Match m where m.homeTeamId = ?1 or m.awayTeamId = ?1")
    public Boolean existsByHomeTeamIdOrAwayTeamId(Long teamId);

    public Boolean existsByMatchdayIdAndHomeTeamIdAndAwayTeamId(Long matchdayId, Long homeTeamId, Long awayTeamId);

    public Match getByMatchdayIdAndHomeTeamIdAndAwayTeamId(Long matchdayId, Long homeTeamId, Long awayTeamId);
}
