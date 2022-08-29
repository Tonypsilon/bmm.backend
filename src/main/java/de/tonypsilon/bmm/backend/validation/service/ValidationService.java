package de.tonypsilon.bmm.backend.validation.service;

import de.tonypsilon.bmm.backend.exception.BadDataException;
import org.apache.commons.validator.routines.EmailValidator;
import org.springframework.lang.Nullable;
import org.springframework.stereotype.Service;

import java.util.Arrays;

/**
 * A service to take over general validation tasks. This is not meant
 * to do business validation that would involve any other services.
 *
 * Note: This service is designed to be self-sufficient, so there is
 * no need to mock it in unit tests.
 */
@Service
public class ValidationService {

    private final EmailValidator emailValidator = EmailValidator.getInstance();

    private final String[] invalidNameCharacters = {";", "@", "\\", "\""};

    /**
     * Validate a date string to not be empty / blank and to contain only
     * alphanumerical symbols or . or -
     * @param date the date to be validated
     */
    public void validateDateString(String date) {
        if(date == null || date.isBlank()) {
            throw new BadDataException("Das Datum darf nicht leer sein!");
        }
        if (!date.matches("[\\w\\-\\.\s]+")) {
            throw new BadDataException("Das Datum enthält ungültige Zeichen!");
        }
    }

    /**
     * Validate a forename or surname to not be empty / blank and to not
     * contain certain special characters.
     * @param name the name to be validated
     */
    public void validateName(@Nullable String name) {
        if (name == null || name.isBlank()) {
            throw new BadDataException("Der Name darf nicht leer sein!");
        }
        if(Arrays.stream(invalidNameCharacters).anyMatch(name::contains)) {
            throw new BadDataException("Der Name enthält ungültige Zeichen!");
        }

    }

    /**
     * Validate an email address to not be null and to be a valid email
     * address format, based on apache commons validation.
     * @param emailAddress the emailAddress to be validated.
     */
    public void validateEmailAddress(@Nullable String emailAddress) {
        if(emailAddress == null || !emailValidator.isValid(emailAddress)) {
            throw new BadDataException("Die E-Mailadresse ist ungültig!");
        }
    }

    public void validateRating(Integer rating) {
        if(rating == null) {
            throw new BadDataException("Das Rating darf nicht leer sein!");
        }
        if (rating <= 0) {
            throw new BadDataException("Das Rating muss positiv sein!");
        }
    }
}
