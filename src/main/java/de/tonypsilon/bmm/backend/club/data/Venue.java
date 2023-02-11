package de.tonypsilon.bmm.backend.club.data;

import org.springframework.lang.NonNull;
import org.springframework.lang.Nullable;

import javax.persistence.*;
import java.util.Optional;

@Entity
public class Venue {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "club_id", nullable = false)
    private Long clubId;

    @Column(nullable = false)
    private String address;

    @Column
    private String hints;

    @NonNull
    public Long getId() {
        return id;
    }

    public void setId(@NonNull Long id) {
        this.id = id;
    }

    @NonNull
    public Long getClubId() {
        return clubId;
    }

    public void setClubId(@NonNull Long clubId) {
        this.clubId = clubId;
    }

    @NonNull
    public String getAddress() {
        return address;
    }

    public void setAddress(@NonNull String address) {
        this.address = address;
    }

    public Optional<String> getHints() {
        return Optional.ofNullable(hints);
    }

    public void setHints(@Nullable String hints) {
        this.hints = hints;
    }
}
