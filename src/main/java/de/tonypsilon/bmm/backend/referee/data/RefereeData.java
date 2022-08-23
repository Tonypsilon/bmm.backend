package de.tonypsilon.bmm.backend.referee.data;

public record RefereeData(Long id,
                          Long seasonId,
                          String forename,
                          String surname,
                          String emailAddress) {
}
