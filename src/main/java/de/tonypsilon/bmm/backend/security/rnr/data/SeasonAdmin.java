package de.tonypsilon.bmm.backend.security.rnr.data;

import jakarta.persistence.*;

@Entity
@IdClass(SeasonAdminKey.class)
public class SeasonAdmin {

    @Id
    private String username;

    @Id
    @Column(name = "season_id")
    private Long seasonId;
}
