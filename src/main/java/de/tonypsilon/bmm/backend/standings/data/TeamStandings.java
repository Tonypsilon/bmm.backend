package de.tonypsilon.bmm.backend.standings.data;

import de.tonypsilon.bmm.backend.match.data.MatchData;
import de.tonypsilon.bmm.backend.team.data.TeamData;
import org.springframework.lang.NonNull;

import java.util.Objects;

public class TeamStandings {
    private final TeamData team;
    private Integer teamPoints;
    private Integer doubledBoardPoints;

    public TeamStandings(@NonNull TeamData teamData) {
        this.team = Objects.requireNonNull(teamData);
        this.teamPoints = 0;
        this.doubledBoardPoints = 0;
    }

    @NonNull
    public Integer getTeamPoints() {
        return teamPoints;
    }

    @NonNull
    public Integer getDoubledBoardPoints() {
        return this.doubledBoardPoints;
    }

    public void addResult(@NonNull Integer teamPoints, @NonNull Integer doubledBoardPoints) {
        this.teamPoints += Objects.requireNonNull(teamPoints);
        this.doubledBoardPoints += Objects.requireNonNull(doubledBoardPoints);
    }

    public void addResult(@NonNull MatchData matchData) {
        Objects.requireNonNull(matchData);
        if (matchData.homeTeamId().equals(this.team.id())) {

        }
    }

    @NonNull
    public TeamData getTeam() {
        return this.team;
    }

}
