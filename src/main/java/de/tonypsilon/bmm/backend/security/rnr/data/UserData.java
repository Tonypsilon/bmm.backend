package de.tonypsilon.bmm.backend.security.rnr.data;

import de.tonypsilon.bmm.backend.security.rnr.Role;

import java.util.Set;

public record UserData(String username,
                       String password,
                       Set<Role> roles,
                       String email,
                       String phone) {
}
