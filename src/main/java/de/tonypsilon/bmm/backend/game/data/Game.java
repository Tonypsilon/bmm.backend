package de.tonypsilon.bmm.backend.game.data;

import de.tonypsilon.bmm.backend.game.service.Result;
import javax.persistence.*;

import java.util.Optional;

@Entity
public class Game {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "match_id", nullable = false)
    private Long matchId;

    @Column(name = "board_number", nullable = false)
    private Integer boardNumber;

    @Column(name = "home_participant_id", nullable = false)
    private Long homeParticipantId;

    @Column(name = "away_participant_id", nullable = false)
    private Long awayParticipantId;

    @Column(name = "played_result_home", nullable = true)
    private String playedResultHome;

    @Column(name = "overruled_result_home", nullable = true)
    private String overruledResultHome;

    @Column(name = "played_result_away", nullable = true)
    private String playedResultAway;

    @Column(name = "overruled_result_away", nullable = true)
    private String overruledResultAway;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Long getMatchId() {
        return matchId;
    }

    public void setMatchId(Long matchId) {
        this.matchId = matchId;
    }

    public Integer getBoardNumber() {
        return boardNumber;
    }

    public void setBoardNumber(Integer boardNumber) {
        this.boardNumber = boardNumber;
    }

    public Long getHomeParticipantId() {
        return homeParticipantId;
    }

    public void setHomeParticipantId(Long homeParticipantId) {
        this.homeParticipantId = homeParticipantId;
    }

    public Long getAwayParticipantId() {
        return awayParticipantId;
    }

    public void setAwayParticipantId(Long awayParticipantId) {
        this.awayParticipantId = awayParticipantId;
    }

    public Optional<Result> getPlayedResultHome() {
        if (playedResultHome == null) {
            return Optional.empty();
        }
        return Optional.of(Result.valueOf(playedResultHome));
    }

    public void setPlayedResultHome(Result playedResultHome) {
        this.playedResultHome = Optional.ofNullable(playedResultHome)
                .map(Result::toString)
                .orElse(null);
    }

    public Optional<Result> getOverruledResultHome() {
        return Optional.ofNullable(overruledResultHome)
                .map(Result::valueOf);
    }

    public void setOverruledResultHome(Result overruledResultHome) {
        this.overruledResultHome = Optional.ofNullable(overruledResultHome)
                .map(Result::toString)
                .orElse(null);
    }

    public Optional<Result> getPlayedResultAway() {
        return Optional.ofNullable(playedResultAway)
                .map(Result::valueOf);
    }

    public void setPlayedResultAway(Result playedResultAway) {
        this.playedResultAway = Optional.ofNullable(playedResultAway)
                .map(Result::toString)
                .orElse(null);
    }

    public Optional<Result> getOverruledResultAway() {
        return Optional.ofNullable(overruledResultAway)
                .map(Result::valueOf);
    }

    public void setOverruledResultAway(Result overruledResultAway) {
        this.overruledResultAway = Optional.ofNullable(overruledResultAway)
                .map(Result::toString)
                .orElse(null);
    }
}
