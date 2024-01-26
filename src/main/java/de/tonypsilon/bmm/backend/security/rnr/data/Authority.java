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
    @Column(name = "authority")
    private Role theAuthority;

    @NonNull
    public User getUser() {
        return user;
    }

    public void setUser(@NonNull User user) {
        this.user = user;
    }

    @NonNull
    public Role getTheAuthority() {
        return theAuthority;
    }

    public void setTheAuthority(@NonNull Role authority) {
        this.theAuthority = authority;
    }

    @Override
    public boolean equals(Object other) {
        if(this == other) {
            return true;
        }
        if(!(other instanceof Authority otherAuthority)) {
            return false;
        }
        return Objects.equals(this.user.getUsername(), otherAuthority.user.getUsername())
                && Objects.equals(this.theAuthority, otherAuthority.theAuthority);
    }

    @Override
    public int hashCode() {
        return Objects.hash(this.user.getUsername(), this.theAuthority.name());
    }
}
