package de.tonypsilon.bmm.backend.division.data;

import jakarta.persistence.*;
import org.springframework.lang.NonNull;

@Entity
public class Division {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(unique = false, nullable = false)
    private String name;

    @Column(unique = false, nullable = false)
    private Integer level;

    @Column(unique = false, nullable = false, name = "number_of_boards")
    private Integer numberOfBoards;

    @Column(unique = false, nullable = false, name = "season_id")
    private Long seasonId;

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
}
