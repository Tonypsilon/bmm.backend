package de.tonypsilon.bmm.backend.security.rnr.data;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.IdClass;
import org.springframework.lang.NonNull;

@Entity
@IdClass(TeamAdminKey.class)
public class TeamAdmin {

    @Id
    private String username;

    @Id
    @Column(name = "team_id")
    private Long teamId;

    @NonNull
    public String getUsername() {
        return username;
    }

    public void setUsername(@NonNull String username) {
        this.username = username;
    }

    @NonNull
    public Long getTeamId() {
        return teamId;
    }

    public void setTeamId(@NonNull Long teamId) {
        this.teamId = teamId;
    }
}
