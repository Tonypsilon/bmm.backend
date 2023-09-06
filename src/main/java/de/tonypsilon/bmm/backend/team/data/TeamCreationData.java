package de.tonypsilon.bmm.backend.team.data;

public record TeamCreationData(Long organizationId,
                               Integer number,
                               Long venueId,
                               String name,
                               String captainUsername) {
}
