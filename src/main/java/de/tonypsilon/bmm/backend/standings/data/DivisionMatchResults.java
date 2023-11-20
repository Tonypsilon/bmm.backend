package de.tonypsilon.bmm.backend.standings.data;

import de.tonypsilon.bmm.backend.match.data.MatchData;
import org.springframework.lang.NonNull;

import java.util.*;

/**
 * All results of a division, structured by teams.
 *
 * <b>Note:</b> It is assumed that all teams play each other at most once.
 */
public class DivisionMatchResults {

    private final Map<Long, Set<MatchData>> results;

    public DivisionMatchResults(@NonNull List<Long> teamIds) {
        this.results = new HashMap<>(teamIds.size());
        for(Long teamId : teamIds) {
            results.put(teamId, new HashSet<>());
        }
    }

    @NonNull
    public Set<MatchData> getForTeam(Long teamId) {
        return Optional.ofNullable(results.get(teamId)).orElseGet(HashSet::new);
    }

    public void addMatch(Long teamId, MatchData matchData) {
        Optional.ofNullable(results.get(teamId))
                .orElseThrow(() -> new UnsupportedOperationException("Can't add for non existing team!"))
                .add(matchData);
    }

    public Optional<MatchData> findMatch(Long teamId, Long otherTeamId) {
        return Optional.ofNullable(results.get(teamId))
                .flatMap(matchData -> matchData.stream()
                        .filter(md -> matchesTeamIds(teamId, otherTeamId, md))
                        .findAny());
    }

    private boolean matchesTeamIds(Long team1Id, Long team2Id, MatchData matchData) {
        return Objects.equals(team1Id, matchData.homeTeamId()) && Objects.equals(team2Id, matchData.awayTeamId())
                || Objects.equals(team2Id, matchData.homeTeamId()) && Objects.equals(team1Id, matchData.awayTeamId());
    }
}
