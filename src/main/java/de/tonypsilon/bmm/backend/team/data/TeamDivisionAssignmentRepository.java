package de.tonypsilon.bmm.backend.team.data;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Repository;

import java.util.Optional;

@Repository
public interface TeamDivisionAssignmentRepository
        extends JpaRepository<TeamDivisionAssignment, TeamDivisionAssignmentKey> {

    boolean existsByDivisionIdAndNumber(@NonNull Long divisionId, @NonNull Integer number);

    boolean existsByTeamId(@NonNull Long teamId);

    Optional<TeamDivisionAssignment> findByTeamIdAndDivisionId(@NonNull Long teamId, @NonNull Long divisionId);

    Optional<TeamDivisionAssignment> findByTeamId(@NonNull Long teamId);
}
