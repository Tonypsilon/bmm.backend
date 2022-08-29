package de.tonypsilon.bmm.backend.teamcaptain.data;

public record CreateTeamCaptainData(Long teamId,
                                    String emailAddress,
                                    String phoneNumber,
                                    String forename,
                                    String surname) {
}
