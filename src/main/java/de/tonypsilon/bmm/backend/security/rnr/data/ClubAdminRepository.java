package de.tonypsilon.bmm.backend.security.rnr.data;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.Optional;

@Repository
public interface ClubAdminRepository extends JpaRepository<ClubAdmin, ClubAdminKey> {

    public Boolean existsByClubIdAndUsername(Long clubId, String username);

    public ClubAdmin getByClubIdAndUsername(Long clubId, String username);

    public Optional<ClubAdmin> findByClubIdAndUsername(Long clubId, String username);
}
