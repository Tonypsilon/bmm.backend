package de.tonypsilon.bmm.backend.participationeligibility.data;

public record ParticipationEligibilityData(Long id,
                                           Long seasonId,
                                           Long clubId,
                                           String forename,
                                           String surname,
                                           Integer pkz,
                                           Integer dwz) {
}
