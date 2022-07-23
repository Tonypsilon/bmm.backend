package de.tonypsilon.bmm.backend.participationeligibility.data;

import java.util.Optional;

public record ParticipationEligibilityCreationData(Long seasonId,
                                                   Long clubId,
                                                   String forename,
                                                   String surname,
                                                   Integer pkz,
                                                   Optional<Integer> dwz) {
}
