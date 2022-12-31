package de.tonypsilon.bmm.backend.security.rnr.data;

public record ChangePasswordData(String username, String oldPassword, String newPassword) {
}
