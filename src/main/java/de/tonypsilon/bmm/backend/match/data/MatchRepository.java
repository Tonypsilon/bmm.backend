package de.tonypsilon.bmm.backend.match.data;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.util.Set;

@Repository
public interface MatchRepository extends JpaRepository<Match, Long> {

    Set<Match> findByMatchdayId(Long matchdayId);

    @Query(value = "select m from Match m where m.matchdayId = ?1 and (m.homeTeamId = ?2 or m.awayTeamId = ?3)")
    Boolean existsByMatchdayIdAndHomeTeamIdOrAwayTeamId(Long matchdayId, Long homeTeamId, Long awayTeamId);

    default Boolean existsByMatchdayIdAndHomeTeamIdOrAwayTeamId(Long matchdayId, Long teamId) {
        return existsByMatchdayIdAndHomeTeamIdOrAwayTeamId(matchdayId, teamId, teamId);
    }

    Match getByMatchdayIdAndHomeTeamIdAndAwayTeamId(Long matchdayId, Long homeTeamId, Long awayTeamId);
}
