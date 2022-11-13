package de.tonypsilon.bmm.backend.team.data;

import javax.persistence.*;
import org.springframework.lang.NonNull;

import java.util.Optional;

@Entity
public class Team {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "organization_id", unique = false, nullable = false)
    private Long organizationId;

    @Column(unique = false, nullable = false)
    private Integer number;

    @Column(name = "division_id", unique = false, nullable = true)
    private Long divisionId;

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

    public Optional<Long> getDivisionId() {
        return Optional.ofNullable(divisionId);
    }

    public void setDivisionId(Long divisionId) {
        this.divisionId = divisionId;
    }
}
