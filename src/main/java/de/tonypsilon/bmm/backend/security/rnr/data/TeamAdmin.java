package de.tonypsilon.bmm.backend.security.rnr.data;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.IdClass;

@Entity
@IdClass(TeamAdminKey.class)
public class TeamAdmin {

    @Id
    private String username;

    @Id
    @Column(name = "team_id")
    private Long teamId;
}
