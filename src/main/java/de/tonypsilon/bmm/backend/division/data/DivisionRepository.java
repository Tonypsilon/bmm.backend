package de.tonypsilon.bmm.backend.division.data;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.Collection;
import java.util.List;

@Repository
public interface DivisionRepository extends JpaRepository<Division, Long> {

    public Collection<Division> findBySeasonIdIn(List<Long> seasonIds);
}
