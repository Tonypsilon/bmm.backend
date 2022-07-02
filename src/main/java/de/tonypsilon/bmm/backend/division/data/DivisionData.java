package de.tonypsilon.bmm.backend.division.data;

import de.tonypsilon.bmm.backend.season.data.SeasonData;

public record DivisionData(Long id,
                           String name,
                           Integer level,
                           Integer numberOfBoards,
                           Long seasonId) {
}
