package de.tonypsilon.bmm.backend.referee.data;

public record RefereeCreationData(Long seasonId,
                                  String forename,
                                  String surname,
                                  String emailAddress) {
}
