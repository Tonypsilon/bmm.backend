package de.tonypsilon.bmm.backend.season.data;

import de.tonypsilon.bmm.backend.season.service.SeasonStage;

public record SeasonStageChangeData(String seasonName, SeasonStage stage) {
}
