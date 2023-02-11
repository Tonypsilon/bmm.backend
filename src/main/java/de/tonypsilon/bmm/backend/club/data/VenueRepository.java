package de.tonypsilon.bmm.backend.club.data;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.Set;

@Repository
public interface VenueRepository extends JpaRepository<Venue, Long> {

    Set<Venue> findByClubId(Long clubId);

    boolean existsByClubIdAndAddress(Long clubId, String address);

    Venue getByClubIdAndAddress(Long clubId, String address);
}
