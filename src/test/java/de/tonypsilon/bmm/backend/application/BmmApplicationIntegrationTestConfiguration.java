package de.tonypsilon.bmm.backend.application;

import org.springframework.boot.context.properties.ConfigurationProperties;

@ConfigurationProperties(prefix = "de.tonypsilon.bmm.integrationtest")
public record BmmApplicationIntegrationTestConfiguration (

    String adminUsername,
    String adminPassword,

    String seasonAdminUsername,
    String seasonAdminPassword,

    String clubAdminUsername,
    String clubAdminPassword,

    String teamAdminUsername,
    String teamAdminPassword
) { }
