package de.tonypsilon.bmm.backend.security.rnr.data;

import java.io.Serializable;

public record TeamAdminKey(String username, Long teamId) implements Serializable {

    public String getUsername() {
        return username;
    }

    public Long getTeamId() {
        return teamId;
    }
}
