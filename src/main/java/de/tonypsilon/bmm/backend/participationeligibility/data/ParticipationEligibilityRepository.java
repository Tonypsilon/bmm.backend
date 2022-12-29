package de.tonypsilon.bmm.backend.participationeligibility.data;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.Collection;

@Repository
public interface ParticipationEligibilityRepository extends JpaRepository<ParticipationEligibility, Long> {

    Boolean existsBySeasonIdAndClubIdAndPkz(Long seasonId, Long clubId, Integer pkz);

    ParticipationEligibility getBySeasonIdAndClubIdAndPkz(Long seasonId, Long clubId, Integer pkz);

    Collection<ParticipationEligibility> getBySeasonId(Long seasonId);

    Collection<ParticipationEligibility> getBySeasonIdAndClubId(Long seasonId, Long clubId);
}
