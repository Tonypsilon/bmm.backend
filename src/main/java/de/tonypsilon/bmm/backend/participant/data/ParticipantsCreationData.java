package de.tonypsilon.bmm.backend.participant.data;

import java.util.Collection;

public record ParticipantsCreationData(
        Long teamId,
        Collection<ParticipantCreationData> participantCreationDataCollection) {
}
