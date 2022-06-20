package de.tonypsilon.bmm.backend.security.rnr.data;

import java.io.Serializable;

public record ClubAdminKey(String username, Long clubId) implements Serializable {

    public String getUsername() {
        return username;
    }

    public Long getClubId() {
        return clubId;
    }
}
