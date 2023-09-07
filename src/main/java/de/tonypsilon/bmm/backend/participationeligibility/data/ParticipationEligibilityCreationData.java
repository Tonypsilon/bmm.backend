package de.tonypsilon.bmm.backend.participationeligibility.data;

public record ParticipationEligibilityCreationData(Long seasonId,
                                                   Long clubId,
                                                   String forename,
                                                   String surname,
                                                   Integer pkz,
                                                   Integer dwz) {
}
