package de.tonypsilon.bmm.backend.teamcaptain.data;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface TeamCaptainRepository extends JpaRepository<TeamCaptain, Long> {

    public Boolean existsByTeamId(Long teamId);

    public TeamCaptain getByTeamId(Long teamId);
}
