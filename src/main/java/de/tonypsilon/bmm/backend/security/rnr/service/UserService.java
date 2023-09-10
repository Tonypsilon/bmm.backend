package de.tonypsilon.bmm.backend.security.rnr.service;

import com.google.common.base.CharMatcher;
import de.tonypsilon.bmm.backend.exception.AlreadyExistsException;
import de.tonypsilon.bmm.backend.exception.BadDataException;
import de.tonypsilon.bmm.backend.exception.NotFoundException;
import de.tonypsilon.bmm.backend.security.rnr.Role;
import de.tonypsilon.bmm.backend.security.rnr.data.*;
import de.tonypsilon.bmm.backend.validation.service.ValidationService;
import org.springframework.lang.NonNull;
import org.springframework.lang.Nullable;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.stream.Collectors;

@Service
public class UserService {

    private final UserRepository userRepository;
    private final ValidationService validationService;
    private final PasswordEncoder passwordEncoder;

    public UserService(final UserRepository userRepository,
                       final ValidationService validationService,
                       final PasswordEncoder passwordEncoder) {
        this.userRepository = userRepository;
        this.validationService = validationService;
        this.passwordEncoder = passwordEncoder;
    }

    @Transactional
    @NonNull
    public UserData createUser(UserData createUserData) {
        validationService.validateName(createUserData.username());
        validationService.validateAscii(createUserData.username(), "Der Benutzername");
        if (userRepository.existsById(createUserData.username())) {
            throw new AlreadyExistsException("Es gibt bereits einen Benutzer mit dem Benutzernamen %s!"
                    .formatted(createUserData.username()));
        }
        validatePassword(createUserData.password());
        validationService.validateEmailAddress(createUserData.email());

        User user = new User();
        user.setUsername(createUserData.username());
        user.setPassword(passwordEncoder.encode(createUserData.password()));
        user.setEnabled(Boolean.TRUE);
        if (createUserData.roles() != null) {
            for (Role role : createUserData.roles()) {
                Authority authority = new Authority();
                authority.setUser(user);
                authority.setTheAuthority(role);
                user.addAuthority(authority);
            }
        } else {
            Authority authority = new Authority();
            authority.setUser(user);
            authority.setTheAuthority(Role.USER);
            user.addAuthority(authority);
        }
        user.setEmail(createUserData.email());
        user.setPhone(createUserData.phone());
        userRepository.save(user);

        return userToUserData(getByUsername(createUserData.username()));
    }

    @Transactional
    @NonNull
    public UserData changePassword(ChangePasswordData changePasswordData) {
        User user = getByUsername(changePasswordData.username());
        if(!passwordEncoder.matches(changePasswordData.oldPassword(), user.getPassword())) {
            throw new BadDataException("Das alte Passwort ist nicht korrekt!");
        }
        validatePassword(changePasswordData.newPassword());
        user.setPassword(passwordEncoder.encode(changePasswordData.newPassword()));
        userRepository.save(user);

        return userToUserData(getByUsername(changePasswordData.username()));
    }

    /**
     * Assigns the given role to the given user if the user does not yet have it.
     * If the user already has the role, nothing happens.
     * @param username of the user
     * @param role to be assigned
     * @return the updated user
     */
    @Transactional
    @NonNull
    public UserData assignRoleToUser(@NonNull String username, @NonNull Role role) {
        User user = getByUsername(username);
        if (user.getAuthorities().stream()
                .map(Authority::getTheAuthority)
                .noneMatch(role::equals)) {
            Authority authority = new Authority();
            authority.setUser(user);
            authority.setTheAuthority(role);
            user.addAuthority(authority);
            userRepository.save(user);
        }
        return userToUserData(getByUsername(username));
    }

    public UserData getUserDataByUsername(@NonNull String username) {
        return userToUserData(getByUsername(username));
    }

    @NonNull
    private User getByUsername(@NonNull String username) {
        return userRepository.findByUsername(username)
                .orElseThrow(() -> new NotFoundException("Es gibt keinen Benutzer mit dem Benutzernamen %s!"
                        .formatted(username)));
    }

    /*
     * For security reasons, password will always be set to null.
     * Enabled is not used functionally.
     */
    @NonNull
    private UserData userToUserData(@NonNull User user) {
        return new UserData(user.getUsername(),
                null,
                user.getAuthorities().stream()
                        .map(Authority::getTheAuthority)
                        .collect(Collectors.toSet()),
                user.getEmail(),
                user.getPhone()
        );
    }

    /**
     * For now, the only restriction is that the password must have at least length 6.
     * @param password to be validated
     */
    private void validatePassword(@Nullable String password) {
        if (password == null || password.isBlank()) {
            throw new BadDataException("Das Passwort darf nicht leer sein!");
        }
        if (password.length() <6 ) {
            throw new BadDataException("Das Passwort muss mindestens sechs Zeichen lang sein!");
        }
        validationService.validateAscii(password, "Das Passwort");
    }
}
