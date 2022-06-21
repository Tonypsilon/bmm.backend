package de.tonypsilon.bmm.backend.club.data;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface ClubRepository extends JpaRepository<Club, Long> {

    public Boolean existsByName(String name);

    public Boolean existsByZps(Integer zps);

    public Club getByName(String name);
}
