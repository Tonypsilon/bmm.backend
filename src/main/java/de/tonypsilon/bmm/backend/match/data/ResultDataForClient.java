package de.tonypsilon.bmm.backend.match.data;

public record ResultDataForClient(ParticipantDataForClient homeParticipant,
                                  ParticipantDataForClient awayParticipant,
                                  String result) {
}
