package de.tonypsilon.bmm.backend.divisionsetup.data;

import de.tonypsilon.bmm.backend.datatypes.IdAndLabel;
import de.tonypsilon.bmm.backend.division.data.DivisionData;
import de.tonypsilon.bmm.backend.team.data.TeamDivisionLinkData;

import java.util.List;

public record DivisionSetupFoundationData(
        List<DivisionData> availableDivisions,
        List<IdAndLabel> availableTeams,
        List<TeamDivisionLinkData> currentLinks
) {
}
