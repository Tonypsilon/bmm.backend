package de.tonypsilon.bmm.backend.season.data;

import de.tonypsilon.bmm.backend.season.service.SeasonStage;

public record SeasonData(Long id,
                         String name,
                         SeasonStage stage) {
}
