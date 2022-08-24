package de.tonypsilon.bmm.backend.referee.data;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface RefereeRepository extends JpaRepository<Referee, Long> {

    public List<Referee> findBySeasonIdOrderBySurnameAsc(Long seasonId);

    public Boolean existsBySeasonIdAndEmailAddress(Long seasonId, String emailAddress);

    public Referee getBySeasonIdAndEmailAddress(Long seasonId, String emailAddress);
}
