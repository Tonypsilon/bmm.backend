package de.tonypsilon.bmm.backend.security.rnr.data;

import org.springframework.lang.NonNull;

import java.io.Serializable;
import java.util.Objects;

public class ClubAdminKey implements Serializable {

    private String username;

    private Long clubId;

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
    public Long getClubId() {
        return clubId;
    }

    public void setClubId(Long clubId) {
        if(this.clubId != null) {
            throw new UnsupportedOperationException("Value must not change!");
        }
        this.clubId = clubId;
    }

    @Override
    public boolean equals(Object other) {
        if (this == other) {
            return true;
        }
        if (other == null || this.getClass() != other.getClass()) {
            return false;
        }
        ClubAdminKey otherClubAdminKey = (ClubAdminKey) other;
        return Objects.equals(this.username, otherClubAdminKey.username)
                && Objects.equals(this.clubId, otherClubAdminKey.clubId);
    }

    @Override
    public int hashCode() {
        return Objects.hash(username, clubId);
    }
}
