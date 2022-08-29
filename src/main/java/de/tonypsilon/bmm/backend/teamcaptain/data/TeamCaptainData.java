package de.tonypsilon.bmm.backend.teamcaptain.data;

public record TeamCaptainData(Long id,
                              Long teamId,
                              String emailAddress,
                              String phoneNumber,
                              String forename,
                              String surname) {
}
