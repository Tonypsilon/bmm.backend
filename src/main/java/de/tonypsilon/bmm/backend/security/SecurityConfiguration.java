package de.tonypsilon.bmm.backend.security;

import org.springframework.context.annotation.Bean;
import org.springframework.security.config.annotation.method.configuration.EnableGlobalMethodSecurity;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.web.SecurityFilterChain;
import org.springframework.security.web.authentication.logout.SecurityContextLogoutHandler;
import org.springframework.security.web.csrf.CookieCsrfTokenRepository;

import static org.springframework.security.config.Customizer.withDefaults;

@EnableWebSecurity
@EnableGlobalMethodSecurity(securedEnabled = true, jsr250Enabled = true)
public class SecurityConfiguration {

    @Bean
    public SecurityFilterChain web(HttpSecurity http) throws Exception {
        return http.csrf(csrf -> csrf.csrfTokenRepository(CookieCsrfTokenRepository.withHttpOnlyFalse()))
                .cors()
                .and().authorizeHttpRequests()
                .antMatchers("/", "/seasons/**", "/divisions/**").permitAll()
                .and().authorizeHttpRequests(authorize -> authorize.anyRequest().authenticated())
                .httpBasic(withDefaults())
                .sessionManagement(session -> session
                        .maximumSessions(1)
                )
                .logout(logout -> logout
                        .logoutUrl("/logout")
                        .addLogoutHandler(securityContextLogoutHandler())
                )
                .build();
    }

    private SecurityContextLogoutHandler securityContextLogoutHandler() {
        SecurityContextLogoutHandler securityContextLogoutHandler = new SecurityContextLogoutHandler();
        securityContextLogoutHandler.setClearAuthentication(true);
        securityContextLogoutHandler.setInvalidateHttpSession(true);
        return securityContextLogoutHandler;
    }
}
