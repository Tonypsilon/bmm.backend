package de.tonypsilon.bmm.backend.division.data;

public record DivisionCreationData(String name,
                                   Integer level,
                                   Integer numberOfBoards,
                                   Long seasonId,
                                   Integer numberOfTeams) {
}
