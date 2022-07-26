package de.tonypsilon.bmm.backend.participant.data;

public record ParticipantCreateData(Long teamId,
                                    Long participationEligibilityId,
                                    Integer number) {
}
