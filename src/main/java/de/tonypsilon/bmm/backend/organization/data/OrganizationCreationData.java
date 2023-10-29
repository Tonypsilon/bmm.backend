package de.tonypsilon.bmm.backend.organization.data;

import java.util.Set;

public record OrganizationCreationData(Long seasonId,
                                       String name,
                                       Integer firstTeamNumber,
                                       Set<Long> clubIds) {
}
