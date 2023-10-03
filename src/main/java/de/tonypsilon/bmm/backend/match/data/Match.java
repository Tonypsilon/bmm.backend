package de.tonypsilon.bmm.backend.match.data;

import javax.persistence.*;
import org.springframework.lang.NonNull;
import org.springframework.lang.Nullable;

import java.util.Optional;

@Entity
@Table(name = "match_")
public class Match {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "matchday_id", nullable = false)
    private Long matchdayId;

    @Column(name = "home_team_id",  nullable = false)
    private Long homeTeamId;

    @Column(name = "away_team_id", nullable = false)
    private Long awayTeamId;

    @Column
    private String date;

    @Column(name = "venue_id")
    private Long venueId;

    @Column(nullable = false)
    @Enumerated(EnumType.STRING)
    private MatchState state;

    /**
     * Note: To avoid strange round off behaviour for half points, points are stored
     * at the doubled amount to guarantee that they are integers.
     */
    @Column(name = "overruled_home_board_half_points")
    private Integer overruledHomeBoardHalfPoints;

    /**
     * Note: To avoid strange round off behaviour for half points, points are stored
     * at the doubled amount to guarantee that they are integers.
     */
    @Column(name = "overruled_away_board_half_points")
    private Integer overruledAwayBoardHalfPoints;

    @Column(name = "referee_id")
    private Long refereeId;

    @NonNull
    public Long getId() {
        return id;
    }

    public void setId(@NonNull Long id) {
        this.id = id;
    }

    @NonNull
    public Long getMatchdayId() {
        return matchdayId;
    }

    public void setMatchdayId(@NonNull Long matchdayId) {
        this.matchdayId = matchdayId;
    }

    @NonNull
    public Long getHomeTeamId() {
        return homeTeamId;
    }

    public void setHomeTeamId(@NonNull Long homeTeamId) {
        this.homeTeamId = homeTeamId;
    }

    @NonNull
    public Long getAwayTeamId() {
        return awayTeamId;
    }

    public void setAwayTeamId(@NonNull Long awayTeamId) {
        this.awayTeamId = awayTeamId;
    }

    @NonNull
    public Optional<String> getDate() {
        return Optional.ofNullable(date);
    }

    public void setDate(@Nullable String date) {
        this.date = date;
    }

    @NonNull
    public Optional<Integer> getOverruledHomeBoardHalfPoints() {
        return Optional.ofNullable(overruledHomeBoardHalfPoints);
    }

    public void setOverruledHomeBoardHalfPoints(@Nullable Integer overruledHomeBoardHalfPoints) {
        this.overruledHomeBoardHalfPoints = overruledHomeBoardHalfPoints;
    }

    @NonNull
    public Optional<Integer> getOverruledAwayBoardHalfPoints() {
        return Optional.ofNullable(overruledAwayBoardHalfPoints);
    }

    public void setOverruledAwayBoardHalfPoints(@Nullable Integer overruledAwayBoardHalfPoints) {
        this.overruledAwayBoardHalfPoints = overruledAwayBoardHalfPoints;
    }

    @NonNull
    public Optional<Long> getRefereeId() {
        return Optional.ofNullable(refereeId);
    }

    public void setRefereeId(@Nullable Long refereeId) {
        this.refereeId = refereeId;
    }

    @NonNull
    public Optional<Long> getVenueId() {
        return Optional.ofNullable(venueId);
    }

    public void setVenueId(@Nullable Long venueId) {
        this.venueId = venueId;
    }

    @NonNull
    public MatchState getState() {
        return state;
    }

    public void setState(@NonNull MatchState state) {
        this.state = state;
    }
}
