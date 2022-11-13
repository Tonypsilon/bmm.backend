package de.tonypsilon.bmm.backend.participationeligibility.data;

import javax.persistence.*;
import org.springframework.lang.NonNull;

import java.util.Optional;

@Entity
public class ParticipationEligibility {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(unique = false, nullable = false, name = "season_id")
    private Long seasonId;

    @Column(unique = false, nullable = false, name = "club_id")
    private Long clubId;

    @Column(unique = false, nullable = false)
    private String forename;

    @Column(unique = false, nullable = false)
    private String surname;

    @Column(unique = false, nullable = false)
    private Integer pkz;

    @Column(unique = false, nullable = true)
    private Integer dwz;

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
    public Long getClubId() {
        return clubId;
    }

    public void setClubId(@NonNull Long clubId) {
        this.clubId = clubId;
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

    @NonNull
    public Integer getPkz() {
        return pkz;
    }

    public void setPkz(@NonNull Integer pkz) {
        this.pkz = pkz;
    }

    @NonNull
    public Optional<Integer> getDwz() {
        return Optional.ofNullable(dwz);
    }

    public void setDwz(Integer dwz) {
        this.dwz = dwz;
    }
}
