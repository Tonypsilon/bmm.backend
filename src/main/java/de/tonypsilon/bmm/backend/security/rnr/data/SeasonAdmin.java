package de.tonypsilon.bmm.backend.security.rnr.data;

import jakarta.persistence.*;
import org.springframework.lang.NonNull;

@Entity
@IdClass(SeasonAdminKey.class)
public class SeasonAdmin {

    @Id
    private String username;

    @Id
    @Column(name = "season_id")
    private Long seasonId;

    @NonNull
    public String getUsername() {
        return username;
    }

    public void setUsername(@NonNull String username) {
        this.username = username;
    }

    @NonNull
    public Long getSeasonId() {
        return seasonId;
    }

    public void setSeasonId(@NonNull Long seasonId) {
        this.seasonId = seasonId;
    }
}
