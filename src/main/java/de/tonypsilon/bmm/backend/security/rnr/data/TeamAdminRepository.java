package de.tonypsilon.bmm.backend.security.rnr.data;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.Optional;

@Repository
public interface TeamAdminRepository extends JpaRepository<TeamAdmin, TeamAdminKey> {

    public Boolean existsByTeamIdAndUsername(Long teamId, String username);

    public TeamAdmin getByTeamIdAndUsername(Long teamId, String username);

    public Optional<TeamAdmin> findByTeamIdAndUsername(Long teamId, String username);
}
