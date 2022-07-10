package de.tonypsilon.bmm.backend.team.data;

import jakarta.persistence.*;
import org.springframework.lang.NonNull;

@Entity
public class Team {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "season_id", unique = false, nullable = false)
    private Long seasonId;

    @Column(name = "club_id", unique = false, nullable = false)
    private Long clubId;

    @Column(unique = false, nullable = false)
    private Integer number;

    @NonNull
    public Long getId() {
        return id;
    }

    public void setId(@NonNull Long id) {
        this.id = id;
    }

    @NonNull
    public Long getSeasonId() {
        return seasonId;
    }

    public void setSeasonId(@NonNull Long seasonId) {
        this.seasonId = seasonId;
    }

    @NonNull
    public Long getClubId() {
        return clubId;
    }

    public void setClubId(@NonNull Long clubId) {
        this.clubId = clubId;
    }

    @NonNull
    public Integer getNumber() {
        return number;
    }

    public void setNumber(@NonNull Integer number) {
        this.number = number;
    }
}
