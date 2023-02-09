package de.tonypsilon.bmm.backend.game.data;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Set;

@Repository
public interface GameRepository extends JpaRepository<Game, Long> {

    List<Game> findByMatchIdOrderByBoardNumberAsc(Long matchId);

    @Query(value = "select g from Game g where g.homeParticipantId = ?1 or g.awayParticipantId = ?1")
    Set<Game> findByParticipantId(Long participantId);

    Game getByMatchIdAndBoardNumber(Long matchId, Integer boardNumber);

    boolean existsByMatchIdAndBoardNumber(Long matchId, Integer boardNumber);
}
