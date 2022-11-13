package de.tonypsilon.bmm.backend.teamcaptain.data;

import javax.persistence.*;
import org.springframework.lang.NonNull;

@Entity
public class TeamCaptain {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "team_id", unique = true, nullable = false)
    private Long teamId;

    @Column(name = "email_address", unique = false, nullable = false)
    private String emailAddress;

    @Column(name = "phone_number", unique = false, nullable = false)
    private String phoneNumber;

    @Column(unique = false, nullable = false)
    private String forename;

    @Column(unique = false, nullable = false)
    private String surname;

    @NonNull
    public Long getId() {
        return id;
    }

    public void setId(@NonNull Long id) {
        this.id = id;
    }

    @NonNull
    public Long getTeamId() {
        return teamId;
    }

    public void setTeamId(@NonNull Long teamId) {
        this.teamId = teamId;
    }

    @NonNull
    public String getEmailAddress() {
        return emailAddress;
    }

    public void setEmailAddress(@NonNull String emailAddress) {
        this.emailAddress = emailAddress;
    }

    @NonNull
    public String getPhoneNumber() {
        return phoneNumber;
    }

    public void setPhoneNumber(@NonNull String phoneNumber) {
        this.phoneNumber = phoneNumber;
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

    public void setSurname(@NonNull String surname) {
        this.surname = surname;
    }
}
