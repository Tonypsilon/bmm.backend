package de.tonypsilon.bmm.backend.referee.data;

import jakarta.persistence.*;
import org.springframework.lang.NonNull;

@Entity
public class Referee {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "season_id", unique = false, nullable = false)
    private Long seasonId;

    @Column(unique = false, nullable = false)
    private String forename;

    @Column(unique = false, nullable = false)
    private String surname;

    @Column(name = "email_address", unique = false, nullable = false)
    private String emailAddress;

    @NonNull
    public Long getId() {
        return id;
    }

    public void setId(@NonNull Long id) {
        this.id = id;
    }

    @NonNull
    public Long getSeasonId() {
        return seasonId;
    }

    public void setSeasonId(@NonNull Long seasonId) {
        this.seasonId = seasonId;
    }

    @NonNull
    public String getForename() {
        return forename;
    }

    public void setForename(@NonNull String forename) {
        this.forename = forename;
    }

    @NonNull
    public String getSurname() {
        return surname;
    }

    public void setSurname(@NonNull String name) {
        this.surname = name;
    }

    @NonNull
    public String getEmailAddress() {
        return emailAddress;
    }

    public void setEmailAddress(@NonNull String emailAddress) {
        this.emailAddress = emailAddress;
    }
}
