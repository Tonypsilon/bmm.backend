package de.tonypsilon.bmm.backend.participant.data;

import javax.persistence.*;
import org.springframework.lang.NonNull;

@Entity
public class Participant {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "team_id", unique = false, nullable = false)
    private Long teamId;

    @Column(name = "participation_eligibility_id", unique = false, nullable = false)
    private Long participationEligibilityId;

    @Column(unique = false, nullable = false)
    private Integer number;

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
    public Long getParticipationEligibilityId() {
        return participationEligibilityId;
    }

    public void setParticipationEligibilityId(@NonNull Long participationEligibilityId) {
        this.participationEligibilityId = participationEligibilityId;
    }

    @NonNull
    public Integer getNumber() {
        return number;
    }

    public void setNumber(@NonNull Integer number) {
        this.number = number;
    }
}
