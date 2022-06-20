package de.tonypsilon.bmm.backend.security.rnr.data;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.IdClass;

@Entity
@IdClass(ClubAdminKey.class)
public class ClubAdmin {

    @Id
    private String username;

    @Id
    @Column(name = "club_id")
    private Long clubId;
}
