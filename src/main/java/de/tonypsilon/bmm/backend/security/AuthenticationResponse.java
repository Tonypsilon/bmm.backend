package de.tonypsilon.bmm.backend.security;

import de.tonypsilon.bmm.backend.datatypes.IdAndLabel;

import java.util.List;

public record AuthenticationResponse(String username,
                                     boolean isAdmin,
                                     List<IdAndLabel> seasons,
                                     List<IdAndLabel> clubs,
                                     List<IdAndLabel> organizations,
                                     List<IdAndLabel> teams,
                                     List<IdAndLabel> matches) {
}
