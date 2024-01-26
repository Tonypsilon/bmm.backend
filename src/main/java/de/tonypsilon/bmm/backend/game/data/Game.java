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

    @Column(name = "played_result_home")
    @Enumerated(EnumType.STRING)
    private Result playedResultHome;

    @Column(name = "overruled_result_home")
    @Enumerated(EnumType.STRING)
    private Result overruledResultHome;

    @Column(name = "played_result_away")
    @Enumerated(EnumType.STRING)
    private Result playedResultAway;

    @Column(name = "overruled_result_away")
    @Enumerated(EnumType.STRING)
    private Result overruledResultAway;

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
        return Optional.ofNullable(playedResultHome);
    }

    public void setPlayedResultHome(Result playedResultHome) {
        this.playedResultHome = playedResultHome;
    }

    public Optional<Result> getOverruledResultHome() {
        return Optional.ofNullable(overruledResultHome);
    }

    public void setOverruledResultHome(Result overruledResultHome) {
        this.overruledResultHome = (overruledResultHome);
    }

    public Optional<Result> getPlayedResultAway() {
        return Optional.ofNullable(playedResultAway);
    }

    public void setPlayedResultAway(Result playedResultAway) {
        this.playedResultAway = playedResultAway;
    }

    public Optional<Result> getOverruledResultAway() {
        return Optional.ofNullable(overruledResultAway);
    }

    public void setOverruledResultAway(Result overruledResultAway) {
        this.overruledResultAway = overruledResultAway;
    }
}
