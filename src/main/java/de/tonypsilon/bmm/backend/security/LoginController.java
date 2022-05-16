package de.tonypsilon.bmm.backend.security;

import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;

import java.security.Principal;

@RestController
public class LoginController {

    @GetMapping(value = "/user", produces = MediaType.APPLICATION_JSON_VALUE)
    public UsernameResponse user(Principal user) {
        return new UsernameResponse(user.getName());
    }

    @GetMapping(value = "/administration/logout")
    public void logout() {

    }
}
