package de.tonypsilon.bmm.backend.participant.data;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.Collection;
import java.util.List;

@Repository
public interface ParticipantRepository extends JpaRepository<Participant, Long> {

    Collection<Participant> findByTeamId(Long teamId);

    Boolean existsByParticipationEligibilityId(Long participationEligibilityId);

    Participant getByTeamIdAndNumber(Long teamId, Integer number);

    Boolean existsByTeamIdAndNumber(Long teamId, Integer number);

    List<Participant> getByTeamIdOrderByNumberAsc(Long teamId);
}
