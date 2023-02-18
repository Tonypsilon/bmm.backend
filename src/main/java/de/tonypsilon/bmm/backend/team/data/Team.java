package de.tonypsilon.bmm.backend.team.data;

import javax.persistence.*;
import org.springframework.lang.NonNull;

@Entity
public class Team {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "organization_id", unique = false, nullable = false)
    private Long organizationId;

    @Column(unique = false, nullable = false)
    private Integer number;

    @Column(nullable = false)
    private Long venueId;

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
}
