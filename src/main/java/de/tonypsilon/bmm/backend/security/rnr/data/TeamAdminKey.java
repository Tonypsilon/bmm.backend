package de.tonypsilon.bmm.backend.security.rnr.data;

import org.springframework.lang.NonNull;

import java.io.Serializable;
import java.util.Objects;

public class TeamAdminKey implements Serializable {

    private String username;

    private Long teamId;

    @NonNull
    public String getUsername() {
        return username;
    }

    public void setUsername(@NonNull String username) {
        if (this.username != null) {
            throw new UnsupportedOperationException("Value must not change!");
        }
        this.username = username;
    }

    @NonNull
    public Long getTeamId() {
        return teamId;
    }

    public void setTeamId(@NonNull Long teamId) {
        if (this.teamId != null) {
            throw new UnsupportedOperationException("Value must not change!");
        }
        this.teamId = teamId;
    }

    @Override
    public boolean equals(Object other) {
        if (this == other) {
            return true;
        }
        if (other == null || this.getClass() != other.getClass()) {
            return false;
        }
        TeamAdminKey otherTeamAdminKey = (TeamAdminKey) other;
        return Objects.equals(this.username, otherTeamAdminKey.username)
                && Objects.equals(this.teamId, otherTeamAdminKey.teamId);
    }

    @Override
    public int hashCode() {
        return Objects.hash(username, teamId);
    }
}
