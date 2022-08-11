package de.tonypsilon.bmm.backend.participant.data;

public record ParticipantCreationData(Long teamId,
                                      Long participationEligibilityId,
                                      Integer number) {
}
