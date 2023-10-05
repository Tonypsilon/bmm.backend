package de.tonypsilon.bmm.backend.match.data;

import java.util.List;

public record PutResultsData(List<GameDataForClient> games,
                             Boolean closeMatch) {
}
