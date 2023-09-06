package de.tonypsilon.bmm.backend.team.data;

public record TeamData(Long id,
                       Long organizationId,
                       Integer number,
                       Long venueId,
                       String name,
                       String captainUsername) {
}
