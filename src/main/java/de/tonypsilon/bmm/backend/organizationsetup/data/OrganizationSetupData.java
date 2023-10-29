package de.tonypsilon.bmm.backend.organizationsetup.data;

import de.tonypsilon.bmm.backend.participationeligibility.data.ParticipationEligibilityData;

import java.util.List;

public record OrganizationSetupData(List<ParticipationEligibilityData> availablePlayers,
                                    List<TeamSetupData> teams,
                                    Integer firstTeamNumber) {
}
