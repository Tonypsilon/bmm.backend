package de.tonypsilon.bmm.backend.organization.data;

import jakarta.persistence.*;
import org.springframework.lang.NonNull;

import java.util.Set;

@Entity
public class Organization {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "season_id", unique = false, nullable = false)
    private Long seasonId;

    @Column(unique = false, nullable = false)
    private String name;

    @OneToMany(mappedBy = "organization")
    private Set<OrganizationMember> organizationMembers;

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
    public String getName() {
        return name;
    }

    public void setName(@NonNull String name) {
        this.name = name;
    }

    @NonNull
    public Set<OrganizationMember> getOrganizationMembers() {
        return organizationMembers;
    }

    public void setOrganizationMembers(@NonNull Set<OrganizationMember> organizationMembers) {
        this.organizationMembers = organizationMembers;
    }
}
