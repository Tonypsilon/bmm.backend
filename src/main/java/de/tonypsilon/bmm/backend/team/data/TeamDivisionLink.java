package de.tonypsilon.bmm.backend.team.data;

import org.springframework.lang.NonNull;

import javax.persistence.*;

@Entity
@IdClass(TeamDivisionLinkKey.class)
@Table(name = "teamdivisionlink")
public class TeamDivisionLink {

    @Id
    @Column(name = "team_id", nullable = false)
    private Long teamId;

    @Id
    @Column(name = "division_id", nullable = false)
    private Long divisionId;

    @Column(nullable = false)
    private Integer number;

    @NonNull
    public Long getTeamId() {
        return teamId;
    }

    public void setTeamId(@NonNull Long teamId) {
        this.teamId = teamId;
    }

    @NonNull
    public Long getDivisionId() {
        return divisionId;
    }

    public void setDivisionId(@NonNull Long divisionId) {
        this.divisionId = divisionId;
    }

    @NonNull
    public Integer getNumber() {
        return number;
    }

    public void setNumber(@NonNull Integer number) {
        this.number = number;
    }
}
