package de.tonypsilon.bmm.backend.referee.data;

public record CreateRefereeData(Long seasonId,
                                String forename,
                                String surname,
                                String emailAddress) {
}
