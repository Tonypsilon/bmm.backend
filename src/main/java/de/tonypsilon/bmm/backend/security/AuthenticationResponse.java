package de.tonypsilon.bmm.backend.security;

import de.tonypsilon.bmm.backend.club.data.ClubData;
import de.tonypsilon.bmm.backend.season.data.SeasonData;
import de.tonypsilon.bmm.backend.team.data.TeamData;

import java.util.List;

public record AuthenticationResponse(String username,
                                     boolean isAdmin,
                                     List<ClubData> clubs,
                                     List<TeamData> teams,
                                     List<SeasonData> seasons) {
}
