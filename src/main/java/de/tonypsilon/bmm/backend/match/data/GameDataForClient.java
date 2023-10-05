package de.tonypsilon.bmm.backend.match.data;

public record GameDataForClient(ParticipantDataForClient homeParticipant,
                                ParticipantDataForClient awayParticipant,
                                ResultDataForClient result) {
}
