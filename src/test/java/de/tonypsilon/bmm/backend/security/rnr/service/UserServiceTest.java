package de.tonypsilon.bmm.backend.security.rnr.service;

import de.tonypsilon.bmm.backend.exception.BadDataException;
import de.tonypsilon.bmm.backend.security.SecurityConfiguration;
import de.tonypsilon.bmm.backend.security.rnr.data.User;
import de.tonypsilon.bmm.backend.security.rnr.data.UserData;
import de.tonypsilon.bmm.backend.security.rnr.data.UserRepository;
import de.tonypsilon.bmm.backend.validation.service.ValidationService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullAndEmptySource;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.crypto.factory.PasswordEncoderFactories;
import org.springframework.security.crypto.password.PasswordEncoder;

import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

class UserServiceTest {

    private final UserRepository userRepository = mock(UserRepository.class);
    private final ValidationService validationService = new ValidationService();
    private UserService userService;
    private User user1;
    private final PasswordEncoder passwordEncoder = new SecurityConfiguration().encoder();

    @BeforeEach
    void setUp() {
        userService = new UserService(userRepository, validationService, passwordEncoder);
        user1 = new User();
        user1.setUsername("user");
        user1.setPassword("secret");
        user1.setEnabled(Boolean.TRUE);
    }

    @Test
    void testCreateUserOk() {
        UserData createUserData = new UserData("user", "secret");
        when(userRepository.existsById("user")).thenReturn(Boolean.FALSE);
        when(userRepository.findByUsername("user")).thenReturn(Optional.ofNullable(user1));
        UserData actual = userService.createUser(createUserData);
        verify(userRepository).save(argThat(user -> user.getUsername().equals("user")
                && passwordEncoder.matches("secret", user.getPassword())
                && user.getEnabled().equals(Boolean.TRUE))
        );
    }

    @ParameterizedTest
    @NullAndEmptySource
    void testCreateUserBlankName(String name) {
        UserData createUserData = new UserData(name, "secret");
        BadDataException actualException = assertThrows(BadDataException.class,
                () -> userService.createUser(createUserData));
        assertThat(actualException.getMessage()).isEqualTo("Der Name darf nicht leer sein!");
    }

    @Test
    void testCreateUserAlreadyExists() {

    }

    @Test
    void testCreateUserInvalidPassword() {

    }


}