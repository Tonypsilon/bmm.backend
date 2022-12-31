package de.tonypsilon.bmm.backend.security.rnr.service;

import de.tonypsilon.bmm.backend.security.rnr.data.UserRepository;
import de.tonypsilon.bmm.backend.validation.service.ValidationService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.crypto.password.PasswordEncoder;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.mock;

class UserServiceTest {

    private final UserRepository userRepository = mock(UserRepository.class);
    private final ValidationService validationService = new ValidationService();
    private final PasswordEncoder passwordEncoder = mock(PasswordEncoder.class);
    private UserService userService;

    @BeforeEach
    void setUp() {
        userService = new UserService(userRepository, validationService, passwordEncoder);
    }

    @Test
    void testCreateUserOk() {

    }

    @Test
    void testCreateUserInvalidName() {

    }

    @Test
    void testCreateUserAlreadyExists() {

    }

    @Test
    void testCreateUserInvalidPassword() {

    }


}