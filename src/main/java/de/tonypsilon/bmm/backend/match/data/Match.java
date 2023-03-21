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

    @Column(name = "matchday_id", unique = false, nullable = false)
    private Long matchdayId;

    @Column(name = "home_team_id",  unique = false, nullable = false)
    private Long homeTeamId;

    @Column(name = "away_team_id", unique = false, nullable = false)
    private Long awayTeamId;

    @Column(unique = false, nullable = true)
    private String date;

    @Column(name = "home_team_points", unique = false, nullable = false)
    private Integer homeTeamPoints;

    @Column(name = "away_team_points", unique = false, nullable = false)
    private Integer awayTeamPoints;

    @Column(unique = false, nullable = false)
    private Boolean editable;

    @Column(name = "venue_id")
    private Long venueId;

    /**
     * Note: To avoid strange round off behaviour for half points, points are stored
     * at the doubled amount to guarantee that they are integers.
     */
    @Column(name = "overruled_home_board_half_points", unique = false, nullable = true)
    private Integer overruledHomeBoardHalfPoints;

    /**
     * Note: To avoid strange round off behaviour for half points, points are stored
     * at the doubled amount to guarantee that they are integers.
     */
    @Column(name = "overruled_away_board_half_points", unique = false, nullable = true)
    private Integer overruledAwayBoardHalfPoints;

    @Column(name = "referee_id", unique = false, nullable = true)
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
    public Integer getHomeTeamPoints() {
        return homeTeamPoints;
    }

    public void setHomeTeamPoints(@NonNull Integer homeTeamPoints) {
        this.homeTeamPoints = homeTeamPoints;
    }

    @NonNull
    public Integer getAwayTeamPoints() {
        return awayTeamPoints;
    }

    public void setAwayTeamPoints(@NonNull Integer awayTeamPoints) {
        this.awayTeamPoints = awayTeamPoints;
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
    public Boolean getEditable() {
        return editable;
    }

    public void setEditable(@NonNull Boolean editable) {
        this.editable = editable;
    }

    @NonNull
    public Optional<Long> getVenueId() {
        return Optional.ofNullable(venueId);
    }

    public void setVenueId(@Nullable Long venueId) {
        this.venueId = venueId;
    }
}
