package de.tonypsilon.bmm.backend.security.rnr.service;

import de.tonypsilon.bmm.backend.exception.AlreadyExistsException;
import de.tonypsilon.bmm.backend.exception.BadDataException;
import de.tonypsilon.bmm.backend.exception.NotFoundException;
import de.tonypsilon.bmm.backend.security.rnr.data.*;
import de.tonypsilon.bmm.backend.validation.service.ValidationService;
import org.springframework.lang.NonNull;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

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
        if (userRepository.existsById(createUserData.username())) {
            throw new AlreadyExistsException("Es gibt bereits einen Benutzer mit dem Benutzernamen %s!"
                    .formatted(createUserData.username()));
        }
        validatePassword(createUserData.password());
        User user = new User();
        user.setUsername(createUserData.username());
        user.setPassword(passwordEncoder.encode(createUserData.password()));
        user.setEnabled(Boolean.TRUE);
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
        return new UserData(user.getUsername(), null);
    }

    /**
     * For now, the only restriction is that the password must have at least length 6.
     * @param password
     */
    private void validatePassword(@NonNull String password) {
        if (password == null || password.isBlank()) {
            throw new BadDataException("Das Passwort darf nicht leer sein!");
        }
        if (password.length() <6 ) {
            throw new BadDataException("Das Passwort muss mindestens sechs Zeichen lang sein!");
        }
    }
}
