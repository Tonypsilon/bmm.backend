package de.tonypsilon.bmm.backend.security.rnr.data;

import de.tonypsilon.bmm.backend.security.rnr.Role;
import org.springframework.lang.NonNull;

import java.io.Serializable;

public class AuthorityKey implements Serializable {

    private User user;

    private Role authority;

    @NonNull
    public User getUser() {
        return user;
    }

    public void setUser(@NonNull User user) {
        this.user = user;
    }

    @NonNull
    public Role getAuthority() {
        return authority;
    }

    public void setAuthority(@NonNull Role authority) {
        this.authority = authority;
    }
}
