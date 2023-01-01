package de.tonypsilon.bmm.backend.security.rnr.data;

import org.springframework.lang.NonNull;

import javax.persistence.*;
import java.util.*;

@Entity
@Table(name = "users")
public class User {

    @Id
    private String username;

    @Column(unique = false, nullable = false)
    private String password;

    @Column(unique = false, nullable = false)
    private Boolean enabled;

    @OneToMany(mappedBy = "user", cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<Authority> authorities = new HashSet<>();

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
    public String getPassword() {
        return password;
    }

    public void setPassword(@NonNull String password) {
        this.password = password;
    }

    @NonNull
    public Boolean getEnabled() {
        return enabled;
    }

    public void setEnabled(@NonNull Boolean enabled) {
        this.enabled = enabled;
    }

    public void addAuthority(@NonNull Authority authority) {
        this.authorities.add(authority);
        authority.setUser(this);
    }

    public void removeAuthority(@NonNull Authority authority) {
        this.authorities.remove(authority);
    }

    @NonNull
    public Set<Authority> getAuthorities() {
        return authorities;
    }
}
