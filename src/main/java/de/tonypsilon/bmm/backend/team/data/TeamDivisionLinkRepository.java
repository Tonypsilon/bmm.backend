package de.tonypsilon.bmm.backend.team.data;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Repository;

import java.util.Optional;
import java.util.Set;

@Repository
public interface TeamDivisionLinkRepository
        extends JpaRepository<TeamDivisionLink, TeamDivisionLinkKey> {

    boolean existsByDivisionIdAndNumber(@NonNull Long divisionId, @NonNull Integer number);

    boolean existsByTeamId(@NonNull Long teamId);

    Optional<TeamDivisionLink> findByTeamIdAndDivisionId(@NonNull Long teamId, @NonNull Long divisionId);

    Optional<TeamDivisionLink> findByTeamId(@NonNull Long teamId);

    Set<TeamDivisionLink> findByDivisionId(@NonNull Long divisionId);

    void deleteByDivisionId(@NonNull Long divisionId);
}
