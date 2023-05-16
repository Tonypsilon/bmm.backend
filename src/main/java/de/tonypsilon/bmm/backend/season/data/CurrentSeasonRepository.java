package de.tonypsilon.bmm.backend.season.data;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface CurrentSeasonRepository extends JpaRepository<CurrentSeason, Long> {

}
