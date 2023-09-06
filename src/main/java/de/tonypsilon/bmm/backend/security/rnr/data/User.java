package de.tonypsilon.bmm.backend.security.rnr.data;

import org.springframework.lang.NonNull;

import javax.persistence.*;
import java.util.HashSet;
import java.util.Set;

@Entity
@Table(name = "users")
public class User {

    @Id
    private String username;

    @Column(nullable = false)
    private String password;

    @Column(nullable = false)
    private Boolean enabled;

    @OneToMany(mappedBy = "user", cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<Authority> authorities = new HashSet<>();

    @Column(nullable = false)
    private String email;

    @Column
    private String phone;

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

    @NonNull
    public String getEmail() {
        return email;
    }

    public void setEmail(@NonNull String email) {
        this.email = email;
    }

    public String getPhone() {
        return phone;
    }

    public void setPhone(String phone) {
        this.phone = phone;
    }
}
