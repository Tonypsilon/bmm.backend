package de.tonypsilon.bmm.backend.referee.data;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface RefereeRepository extends JpaRepository<Referee, Long> {

    List<Referee> findBySeasonIdOrderBySurnameAsc(Long seasonId);

    Boolean existsBySeasonIdAndEmailAddress(Long seasonId, String emailAddress);

    Referee getBySeasonIdAndEmailAddress(Long seasonId, String emailAddress);
}
