package de.tonypsilon.bmm.backend;

import org.springframework.boot.context.properties.ConfigurationProperties;

@ConfigurationProperties(prefix = "de.tonypsilon.bmm.integrationtest")
public record BmmApplicationIntegrationTestConfiguration (

    String adminUsername,
    String adminUserPassword,

    String seasonAdminUsername,
    String seasonAdminUserPassword,

    String clubAdminUsername,
    String clubAdminUserPassword,

    String teamAdminUsername,
    String teamAdminUserPassword
) { }
