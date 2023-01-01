package de.tonypsilon.bmm.backend.security.rnr;

// Make sure to add all roles from here in the enum Role!
public class Roles {

    private Roles() {}

    public static final String ADMIN = "ADMIN";
    public static final String SEASON_ADMIN = "SEASON_ADMIN";
    public static final String CLUB_ADMIN = "CLUB_ADMIN";
    public static final String TEAM_ADMIN = "TEAM_ADMIN";
    public static final String USER = "USER";

}
