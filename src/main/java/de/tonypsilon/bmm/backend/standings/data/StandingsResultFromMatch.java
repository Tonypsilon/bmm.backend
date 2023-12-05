package de.tonypsilon.bmm.backend.standings.data;

public record StandingsResultFromMatch(Integer homeTeamPoints,
                                       Integer doubledHomeBoardPoints,
                                       Integer awayTeamPoints,
                                       Integer doubledAwayBoardPoints) {
}
