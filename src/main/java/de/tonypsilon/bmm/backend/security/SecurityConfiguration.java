package de.tonypsilon.bmm.backend.security;

import org.springframework.context.ApplicationListener;
import org.springframework.context.annotation.Bean;
import org.springframework.security.authentication.event.AbstractAuthenticationEvent;
import org.springframework.security.authentication.event.LoggerListener;
import org.springframework.security.config.annotation.method.configuration.EnableGlobalMethodSecurity;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.crypto.factory.PasswordEncoderFactories;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.security.web.SecurityFilterChain;
import org.springframework.security.web.authentication.logout.SecurityContextLogoutHandler;

@EnableWebSecurity
@EnableGlobalMethodSecurity(securedEnabled = true, jsr250Enabled = true)
public class SecurityConfiguration {

    @Bean
    public SecurityFilterChain web(HttpSecurity http) throws Exception {
        return http
                .cors().and()
                .csrf().disable()
                .httpBasic()
                .and().authorizeRequests()
                .antMatchers("/", "/seasons/**", "/divisions/**", "/currentseason/**").permitAll()
                .and().authorizeRequests()
                .anyRequest().authenticated()
                .and()
                .sessionManagement(session -> session
                        .maximumSessions(1)
                )
                .logout(logout -> logout
                        .logoutUrl("/administration/logout")
                        .addLogoutHandler(securityContextLogoutHandler())
                )
                .build();
    }

    @Bean
    public PasswordEncoder encoder() {
        return PasswordEncoderFactories.createDelegatingPasswordEncoder();
    }

    private SecurityContextLogoutHandler securityContextLogoutHandler() {
        SecurityContextLogoutHandler securityContextLogoutHandler = new SecurityContextLogoutHandler();
        securityContextLogoutHandler.setClearAuthentication(true);
        securityContextLogoutHandler.setInvalidateHttpSession(true);
        return securityContextLogoutHandler;
    }

    @Bean
    public ApplicationListener<AbstractAuthenticationEvent> loginListener() {
        return new LoggerListener();
    }
}
