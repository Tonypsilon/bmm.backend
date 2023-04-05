package de.tonypsilon.bmm.backend.security.rnr.data;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.Optional;
import java.util.Set;

@Repository
public interface TeamAdminRepository extends JpaRepository<TeamAdmin, TeamAdminKey> {

    Boolean existsByTeamIdAndUsername(Long teamId, String username);

    TeamAdmin getByTeamIdAndUsername(Long teamId, String username);

    Optional<TeamAdmin> findByTeamIdAndUsername(Long teamId, String username);

    Set<TeamAdmin> findByTeamId(Long teamId);
}
