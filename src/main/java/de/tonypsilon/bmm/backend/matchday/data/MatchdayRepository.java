package de.tonypsilon.bmm.backend.matchday.data;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface MatchdayRepository extends JpaRepository<Matchday, Long> {

    Boolean existsByDivisionIdAndRound(Long divisionId, Integer round);

    List<Matchday> findByDivisionIdOrderByRoundAsc(Long divisionId);

    Matchday getByDivisionIdAndRound(Long divisionId, Integer round);

}
