package de.tonypsilon.bmm.backend.security.rnr.data;

import de.tonypsilon.bmm.backend.security.rnr.Role;
import org.springframework.lang.NonNull;

import javax.persistence.*;
import java.util.Objects;

@Entity
@IdClass(AuthorityKey.class)
@Table(name = "authorities")
public class Authority {

    @Id
    @ManyToOne
    @JoinColumn(name = "username")
    private User user;

    @Id
    @Enumerated(EnumType.STRING)
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

    @Override
    public boolean equals(Object other) {
        if(this == other) {
            return true;
        }
        if(!(other instanceof Authority)) {
            return false;
        }
        Authority otherAuthority = (Authority) other;
        return Objects.equals(this.user.getUsername(), otherAuthority.user.getUsername())
                && Objects.equals(this.authority, otherAuthority.authority);
    }

    @Override
    public int hashCode() {
        return Objects.hash(this.user.getUsername(), this.authority.name());
    }
}
