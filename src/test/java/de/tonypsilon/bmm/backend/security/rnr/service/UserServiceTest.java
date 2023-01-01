package de.tonypsilon.bmm.backend.security.rnr.service;

import de.tonypsilon.bmm.backend.exception.AlreadyExistsException;
import de.tonypsilon.bmm.backend.exception.BadDataException;
import de.tonypsilon.bmm.backend.exception.NotFoundException;
import de.tonypsilon.bmm.backend.security.SecurityConfiguration;
import de.tonypsilon.bmm.backend.security.rnr.Role;
import de.tonypsilon.bmm.backend.security.rnr.data.*;
import de.tonypsilon.bmm.backend.validation.service.ValidationService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullAndEmptySource;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.security.crypto.password.PasswordEncoder;

import java.util.Optional;
import java.util.Set;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.*;

class UserServiceTest {

    private final UserRepository userRepository = mock(UserRepository.class);
    private final ValidationService validationService = new ValidationService();
    private UserService userService;
    private User user1;
    private Authority authority1;
    private final PasswordEncoder passwordEncoder = new SecurityConfiguration().encoder();

    @BeforeEach
    void setUp() {
        userService = new UserService(userRepository, validationService, passwordEncoder);
        user1 = new User();
        user1.setUsername("user");
        user1.setPassword(passwordEncoder.encode("secret"));
        user1.setEnabled(Boolean.TRUE);
        authority1 = new Authority();
        authority1.setUser(user1);
        authority1.setAuthority(Role.USER);
    }

    @Test
    void testCreateUserOk() {
        UserData createUserData = new UserData("user", "secret", Set.of(Role.USER));
        when(userRepository.existsById("user")).thenReturn(Boolean.FALSE);
        when(userRepository.findByUsername("user")).thenReturn(Optional.ofNullable(user1));
        UserData actual = userService.createUser(createUserData);
        verify(userRepository).save(argThat(user -> user.getUsername().equals("user")
                && passwordEncoder.matches("secret", user.getPassword())
                && user.getEnabled().equals(Boolean.TRUE)
                && user.getAuthorities().equals(Set.of(authority1)))
        );
        assertThat(actual.username()).isEqualTo("user");
        assertThat(actual.password()).isNull();
    }

    @ParameterizedTest
    @NullAndEmptySource
    void testCreateUserBlankName(String name) {
        UserData createUserData = new UserData(name, "secret", null);
        BadDataException actualException = assertThrows(BadDataException.class,
                () -> userService.createUser(createUserData));
        assertThat(actualException.getMessage()).isEqualTo("Der Name darf nicht leer sein!");
    }

    @Test
    void testCreateUserAlreadyExists() {
        UserData createUserData = new UserData("user", "secret", null);
        when(userRepository.existsById("user")).thenReturn(Boolean.TRUE);
        AlreadyExistsException actualException = assertThrows(AlreadyExistsException.class,
                () -> userService.createUser(createUserData));
        assertThat(actualException.getMessage())
                .isEqualTo("Es gibt bereits einen Benutzer mit dem Benutzernamen user!");
    }

    @ParameterizedTest
    @NullAndEmptySource
    void testCreateUserBlankPassword(String password) {
        UserData createUserData = new UserData("user", password, null);
        when(userRepository.existsById("user")).thenReturn(Boolean.FALSE);
        BadDataException actualException = assertThrows(BadDataException.class,
                () -> userService.createUser(createUserData));
        assertThat(actualException.getMessage()).isEqualTo("Das Passwort darf nicht leer sein!");
    }

    // Use value source for only one value because in the future, more password rules are likely.
    @ParameterizedTest
    @ValueSource(strings = {"short"})
    void testCreateUserInvalidPassword(String password) {
        UserData createUserData = new UserData("user", password, null);
        when(userRepository.existsById("user")).thenReturn(Boolean.FALSE);
        BadDataException actualException = assertThrows(BadDataException.class,
                () -> userService.createUser(createUserData));
        assertThat(actualException.getMessage())
                .isEqualTo("Das Passwort muss mindestens sechs Zeichen lang sein!");
    }

    @Test
    void  testChangePasswordOk() {
        ChangePasswordData changePasswordData = new ChangePasswordData("user", "secret", "secret2");
        User user2 = new User();
        user2.setUsername("user");
        user2.setPassword(passwordEncoder.encode("secret2"));
        user2.setEnabled(Boolean.TRUE);
        when(userRepository.findByUsername("user"))
                .thenReturn(Optional.of(user1))
                .thenReturn(Optional.of(user2));
        UserData actual = userService.changePassword(changePasswordData);
        assertThat(actual.username()).isEqualTo("user");
        assertThat(actual.password()).isNull();
        verify(userRepository).save(argThat(user -> user.getUsername().equals("user")
                && passwordEncoder.matches("secret2", user.getPassword())
                && user.getEnabled().equals(Boolean.TRUE))
        );
    }

    @Test
    void testChangePasswordUserDoesNotExist() {
        ChangePasswordData changePasswordData = new ChangePasswordData("user", "secret", "secret2");
        when(userRepository.findByUsername("user")).thenReturn(Optional.empty());
        NotFoundException actualException = assertThrows(NotFoundException.class,
                () -> userService.changePassword(changePasswordData));
        assertThat(actualException.getMessage())
                .isEqualTo("Es gibt keinen Benutzer mit dem Benutzernamen user!");
    }

    @Test
    void testChangePasswordOldPasswordDoesNotMatch() {
        ChangePasswordData changePasswordData = new ChangePasswordData("user", "wrong", "secret2");
        when(userRepository.findByUsername("user")).thenReturn(Optional.of(user1));
        BadDataException actualException = assertThrows(BadDataException.class,
                () -> userService.changePassword(changePasswordData));
        assertThat(actualException.getMessage()).isEqualTo("Das alte Passwort ist nicht korrekt!");
    }

    @ParameterizedTest
    @NullAndEmptySource
    void testChangePasswordBlankPassword(String password) {
        ChangePasswordData changePasswordData = new ChangePasswordData("user", "secret", password);
        when(userRepository.findByUsername("user")).thenReturn(Optional.of(user1));
        BadDataException actualException = assertThrows(BadDataException.class,
                () -> userService.changePassword(changePasswordData));
        assertThat(actualException.getMessage()).isEqualTo("Das Passwort darf nicht leer sein!");
    }

    // Use value source for only one value because in the future, more password rules are likely.
    @ParameterizedTest
    @ValueSource(strings = {"short"})
    void testChangePasswordInvalidPassword(String password) {
        ChangePasswordData changePasswordData = new ChangePasswordData("user", "secret", password);
        when(userRepository.findByUsername("user")).thenReturn(Optional.of(user1));
        BadDataException actualException = assertThrows(BadDataException.class,
                () -> userService.changePassword(changePasswordData));
        assertThat(actualException.getMessage())
                .isEqualTo("Das Passwort muss mindestens sechs Zeichen lang sein!");
    }

}