package de.tonypsilon.bmm.backend.security.rnr.data;

import org.springframework.lang.NonNull;

import java.io.Serializable;
import java.util.Objects;

public class SeasonAdminKey implements Serializable {

    /**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	private String username;

    private Long seasonId;

    @NonNull
    public String getUsername() {
        return username;
    }

    public void setUsername(@NonNull String username) {
        if(this.username != null) {
            throw new UnsupportedOperationException("Value must not change!");
        }
        this.username = username;
    }

    @NonNull
    public Long getSeasonId() {
        return seasonId;
    }

    public void setSeasonId(@NonNull Long seasonId) {
        if(this.seasonId != null) {
            throw new UnsupportedOperationException("Value must not change!");
        }
        this.seasonId = seasonId;
    }

    @Override
    public boolean equals(Object other) {
        if (this == other) {
            return true;
        }
        if (other == null || this.getClass() != other.getClass()) {
            return false;
        }
        SeasonAdminKey otherSeasonAdminKey = (SeasonAdminKey) other;
        return Objects.equals(this.username, otherSeasonAdminKey.username)
                && Objects.equals(this.seasonId, otherSeasonAdminKey.seasonId);
    }

    @Override
    public int hashCode() {
        return Objects.hash(username, seasonId);
    }
}
