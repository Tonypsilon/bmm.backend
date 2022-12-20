package de.tonypsilon.bmm.backend.security;

import java.util.List;

public record AuthenticationResponse(String username,
                                     List<String> roles,
                                     List<String> clubs,
                                     List<String> teams,
                                     List<String> seasons) {
}
