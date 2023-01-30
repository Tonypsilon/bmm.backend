package de.tonypsilon.bmm.backend.security.rnr;

// Make sure that all roles from Roles are added here!
// The main purpose of this class is the hibernate mapping for the authorities entity.
public enum Role {ADMIN, SEASON_ADMIN, CLUB_ADMIN, TEAM_ADMIN, USER}
