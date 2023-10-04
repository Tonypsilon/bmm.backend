package de.tonypsilon.bmm.backend.match.data;

import de.tonypsilon.bmm.backend.datatypes.IdAndLabel;

import java.util.List;

public record RichMatchData(List<ParticipantDataForClient> availableHomePlayers,
                            List<ParticipantDataForClient> availableAwayPlayers,
                            List<ParticipantDataForClient> selectedHomePlayers,
                            List<ParticipantDataForClient> selectedAwayPlayers,
                            Integer numberOfBoards,
                            List<IdAndLabel> availableReferees,
                            IdAndLabel selectedReferee,
                            List<ResultDataForClient> results) {
}
