package de.tonypsilon.bmm.backend.division.data;

import org.springframework.lang.NonNull;

import javax.persistence.*;

@Entity
public class Division {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(nullable = false)
    private String name;

    @Column(nullable = false)
    private Integer level;

    @Column(nullable = false, name = "number_of_boards")
    private Integer numberOfBoards;

    @Column(nullable = false, name = "season_id")
    private Long seasonId;

    @Column(nullable = false, name = "number_of_teams")
    private Integer numberOfTeams;

    @NonNull
    public Long getId() {
        return id;
    }

    public void setId(@NonNull Long id) {
        this.id = id;
    }

    @NonNull
    public String getName() {
        return name;
    }

    public void setName(@NonNull String name) {
        this.name = name;
    }

    @NonNull
    public Integer getLevel() {
        return level;
    }

    public void setLevel(@NonNull Integer level) {
        this.level = level;
    }

    @NonNull
    public Integer getNumberOfBoards() {
        return numberOfBoards;
    }

    public void setNumberOfBoards(@NonNull Integer numberOfBoards) {
        this.numberOfBoards = numberOfBoards;
    }

    @NonNull
    public Long getSeasonId() {
        return seasonId;
    }

    public void setSeasonId(@NonNull Long seasonId) {
        this.seasonId = seasonId;
    }

    @NonNull
    public Integer getNumberOfTeams() {
        return numberOfTeams;
    }

    public void setNumberOfTeams(@NonNull Integer numberOfTeams) {
        this.numberOfTeams = numberOfTeams;
    }
}
