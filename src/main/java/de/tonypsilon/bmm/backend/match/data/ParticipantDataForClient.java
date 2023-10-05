package de.tonypsilon.bmm.backend.match.data;

public record ParticipantDataForClient(Long id,
                                       String code,
                                       String forename,
                                       String surname,
                                       Integer dwz) {
}
