package de.tonypsilon.bmm.backend.participationeligibility.data;

import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Collection;

public interface ParticipationEligibilityRepository extends JpaRepository<ParticipationEligibility, Long> {

    public Boolean existsBySeasonIdAndClubIdAndPkz(Long seasonId, Long clubId, Integer pkz);

    public ParticipationEligibility getBySeasonIdAndClubIdAndPkz(Long seasonId, Long clubId, Integer pkz);

    public Collection<ParticipationEligibility> getBySeasonId(Long seasonId);

    public Collection<ParticipationEligibility> getBySeasonIdAndClubId(Long seasonId, Long clubId);
}
