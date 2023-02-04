package de.tonypsilon.bmm.backend.team.data;

import java.io.Serializable;
import java.util.Objects;

public class TeamDivisionAssignmentKey implements Serializable {

    /**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	private Long teamId;

    private Long divisionId;

    public Long getTeamId() {
        return teamId;
    }

    public void setTeamId(Long teamId) {
        this.teamId = teamId;
    }

    public Long getDivisionId() {
        return divisionId;
    }

    public void setDivisionId(Long divisionId) {
        this.divisionId = divisionId;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        TeamDivisionAssignmentKey that = (TeamDivisionAssignmentKey) o;
        return teamId.equals(that.teamId) && divisionId.equals(that.divisionId);
    }

    @Override
    public int hashCode() {
        return Objects.hash(teamId, divisionId);
    }
}
