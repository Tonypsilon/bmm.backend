package de.tonypsilon.bmm.backend.club.data;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface ClubRepository extends JpaRepository<Club, Long> {

    Boolean existsByName(String name);

    Boolean existsByZps(Integer zps);

    Club getByName(String name);

    Club getById(Long id);
}
