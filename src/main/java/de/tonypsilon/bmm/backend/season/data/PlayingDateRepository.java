package de.tonypsilon.bmm.backend.season.data;

import java.util.Optional;
import java.util.Set;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface PlayingDateRepository extends JpaRepository<PlayingDate, Long> {
	
	Optional<PlayingDate> findBySeasonIdAndNumber(Long seasonId, Integer number);
	
	boolean existsBySeasonIdAndNumber(Long seasonId, Integer number);
	
	Set<PlayingDate> findBySeasonId(Long seasonId);
}
