package de.tonypsilon.bmm.backend.matchday.data;

import javax.persistence.*;
import org.springframework.lang.NonNull;

@Entity
public class Matchday {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "division_id", unique = false, nullable = false)
    private Long divisionId;

    @Column(unique = false, nullable = false)
    private String date;

    @Column(unique = false, nullable = false)
    private Integer round;

    @NonNull
    public Long getId() {
        return id;
    }

    public void setId(@NonNull Long id) {
        this.id = id;
    }

    @NonNull
    public Long getDivisionId() {
        return divisionId;
    }

    public void setDivisionId(@NonNull Long divisionId) {
        this.divisionId = divisionId;
    }

    @NonNull
    public String getDate() {
        return date;
    }

    public void setDate(@NonNull String date) {
        this.date = date;
    }

    @NonNull
    public Integer getRound() {
        return round;
    }

    public void setRound(@NonNull Integer round) {
        this.round = round;
    }
}
