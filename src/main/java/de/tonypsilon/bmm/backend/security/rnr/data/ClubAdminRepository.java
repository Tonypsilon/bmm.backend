package de.tonypsilon.bmm.backend.security.rnr.data;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.Optional;
import java.util.Set;

@Repository
public interface ClubAdminRepository extends JpaRepository<ClubAdmin, ClubAdminKey> {

    Boolean existsByClubIdAndUsername(Long clubId, String username);

    ClubAdmin getByClubIdAndUsername(Long clubId, String username);

    Optional<ClubAdmin> findByClubIdAndUsername(Long clubId, String username);

    Set<ClubAdmin> findByClubId(Long clubId);

    Set<ClubAdmin> findByUsername(String username);
}
