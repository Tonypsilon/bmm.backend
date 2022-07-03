package de.tonypsilon.bmm.backend.security.rnr.data;

import java.io.Serializable;

public record SeasonAdminKey(String username, Long seasonId) implements Serializable {

    public String getUsername() {
        return username;
    }

    public Long getSeasonId() {
        return seasonId;
    }

}
