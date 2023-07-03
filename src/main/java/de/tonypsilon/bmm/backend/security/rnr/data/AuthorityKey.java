package de.tonypsilon.bmm.backend.security.rnr.data;

import de.tonypsilon.bmm.backend.security.rnr.Role;
import org.springframework.lang.NonNull;

import java.io.Serial;
import java.io.Serializable;
import java.util.Objects;

public class AuthorityKey implements Serializable {

    /**
	 * 
	 */
	@Serial
    private static final long serialVersionUID = 1L;

	private User user;

    private Role theAuthority;

    @NonNull
    public User getUser() {
        return user;
    }

    public void setUser(@NonNull User user) {
        if (this.user != null) {
            throw new UnsupportedOperationException("Value must not change!");
        }
        this.user = user;
    }

    @NonNull
    public Role getTheAuthority() {
        return theAuthority;
    }

    public void setTheAuthority(@NonNull Role authority) {
        if(this.theAuthority != null) {
            throw new UnsupportedOperationException("Value must not change!");
        }
        this.theAuthority = authority;
    }

    @Override
    public boolean equals(Object other) {
        if(this == other) {
            return true;
        }
        if(other == null || this.getClass() != other.getClass()) {
            return false;
        }
        AuthorityKey otherAuthorityKey = (AuthorityKey) other;
        return Objects.equals(this.user.getUsername(), otherAuthorityKey.user.getUsername())
                && this.theAuthority == otherAuthorityKey.theAuthority;
    }

    @Override
    public int hashCode() {
        return Objects.hash(this.user.getUsername(), this.theAuthority);
    }
}
