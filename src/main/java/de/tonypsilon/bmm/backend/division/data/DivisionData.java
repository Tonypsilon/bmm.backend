package de.tonypsilon.bmm.backend.division.data;

public record DivisionData(Long id,
                           String name,
                           Integer level,
                           Integer numberOfBoards,
                           Long seasonId) {
}
