package de.tonypsilon.bmm.backend.security.rnr.data;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.Optional;

@Repository
public interface SeasonAdminRepository extends JpaRepository<SeasonAdmin, SeasonAdminKey> {

    Boolean existsBySeasonIdAndUsername(Long seasonId, String username);

    SeasonAdmin getBySeasonIdAndUsername(Long seasonId, String username);

    Optional<SeasonAdmin> findBySeasonIdAndUsername(Long seasonId, String username);
}
