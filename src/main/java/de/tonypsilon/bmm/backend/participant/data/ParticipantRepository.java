package de.tonypsilon.bmm.backend.participant.data;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.Collection;
import java.util.Optional;

@Repository
public interface ParticipantRepository extends JpaRepository<Participant, Long> {

    public Collection<Participant> findByTeamId(Long teamId);

    public Optional<Participant> findByTeamIdAndNumber(Long teamId, Integer number);
}
