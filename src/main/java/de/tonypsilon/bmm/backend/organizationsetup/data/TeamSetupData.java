package de.tonypsilon.bmm.backend.organizationsetup.data;

import de.tonypsilon.bmm.backend.participationeligibility.data.ParticipationEligibilityData;

import java.util.List;

public record TeamSetupData(Long organizationId,
                            Integer number,
                            Long venueId,
                            String name,
                            List<ParticipationEligibilityData> participants /* participation eligibility ids */ ) {
}