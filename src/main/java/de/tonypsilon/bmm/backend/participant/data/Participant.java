package de.tonypsilon.bmm.backend.participant.data;

import org.springframework.lang.NonNull;

import javax.persistence.*;

@Entity
public class Participant {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "team_id", nullable = false)
    private Long teamId;

    @Column(name = "participation_eligibility_id", nullable = false)
    private Long participationEligibilityId;

    @Column(nullable = false)
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
