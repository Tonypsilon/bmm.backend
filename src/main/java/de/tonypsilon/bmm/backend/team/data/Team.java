package de.tonypsilon.bmm.backend.team.data;

import org.springframework.lang.NonNull;

import javax.persistence.*;

@Entity
public class Team {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "organization_id", nullable = false)
    private Long organizationId;

    @Column(nullable = false)
    private Integer number;

    @Column(name = "venue_id", nullable = false)
    private Long venueId;

    @Column(name = "captain_username", nullable = false)
    private String captainUsername;

    @Column
    private String name;

    @NonNull
    public Long getId() {
        return id;
    }

    public void setId(@NonNull Long id) {
        this.id = id;
    }

    @NonNull
    public Long getOrganizationId() {
        return organizationId;
    }

    public void setOrganizationId(@NonNull Long organizationId) {
        this.organizationId = organizationId;
    }

    @NonNull
    public Integer getNumber() {
        return number;
    }

    public void setNumber(@NonNull Integer number) {
        this.number = number;
    }

    @NonNull
    public Long getVenueId() {
        return venueId;
    }

    public void setVenueId(@NonNull Long venueId) {
        this.venueId = venueId;
    }

    @NonNull
    public String getCaptainUsername() {
        return captainUsername;
    }

    public void setCaptainUsername(@NonNull String captainUsername) {
        this.captainUsername = captainUsername;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }
}
