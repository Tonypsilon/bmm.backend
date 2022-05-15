package de.tonypsilon.bmm.backend.security;

import org.springframework.context.annotation.Bean;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.web.SecurityFilterChain;

@EnableWebSecurity
public class SecurityConfiguration {

    @Bean
    public SecurityFilterChain web(HttpSecurity http) throws Exception {
        return http.cors()
                .and().httpBasic()
                .and()
                .authorizeRequests()
                .antMatchers("/", "/season/**", "division/**").permitAll()
                .anyRequest().authenticated()
                .and()
                .sessionManagement(session -> session
                        .maximumSessions(1)
                )
                .build();
    }
}
