package de.tonypsilon.bmm.backend.organization.data;

import java.util.Set;

public record OrganizationData(Long id,
                               Long seasonId,
                               String name,
                               Integer firstTeamNumber,
                               Set<Long> clubIds) {
}
