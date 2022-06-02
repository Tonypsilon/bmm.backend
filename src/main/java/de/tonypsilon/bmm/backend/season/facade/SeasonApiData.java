package de.tonypsilon.bmm.backend.season.facade;

import de.tonypsilon.bmm.backend.season.service.SeasonStage;

public record SeasonApiData(String name, SeasonStage stage) {
}
