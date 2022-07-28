package de.tonypsilon.bmm.backend.matchday.data;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.Collection;

@Repository
public interface MatchdayRepository extends JpaRepository<Matchday, Long> {

    public Boolean existsByDivisionIdAndRound(Long divisionId, Integer round);

    public Collection<Matchday> findByDivisionId(Long divisionId);

}
