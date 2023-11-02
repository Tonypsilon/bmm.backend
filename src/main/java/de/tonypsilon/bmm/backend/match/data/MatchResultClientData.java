package de.tonypsilon.bmm.backend.match.data;

import java.util.List;

public record MatchResultClientData(String date,
                                    String venueLabel,
                                    String refereeLabel,
                                    String homeTeamLabel,
                                    String awayTeamLabel,
                                    String state,
                                    List<GameDataForClient> games,
                                    String homeTeamResultLabel,
                                    String awayTeamResultLabel) {
}
