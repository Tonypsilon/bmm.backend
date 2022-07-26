package de.tonypsilon.bmm.backend.participant.data;

public record ParticipantData(Long id,
                              Long teamId,
                              Long participationEligibilityId,
                              Integer number) {
}
