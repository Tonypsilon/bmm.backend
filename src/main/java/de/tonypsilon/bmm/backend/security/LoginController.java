package de.tonypsilon.bmm.backend.security;

import org.springframework.http.MediaType;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;

import java.security.Principal;
import java.util.Collections;

@RestController
public class LoginController {

    @GetMapping(value = "/user", produces = MediaType.APPLICATION_JSON_VALUE)
    public AuthenticationResponse user(Principal user) {
        return new AuthenticationResponse(user.getName(),
                SecurityContextHolder.getContext().getAuthentication().getAuthorities().stream().map(GrantedAuthority::getAuthority).toList(),
                Collections.emptyList(),
                Collections.emptyList());
    }

    @GetMapping(value = "/administration/logout")
    public void logout() {

    }
}
