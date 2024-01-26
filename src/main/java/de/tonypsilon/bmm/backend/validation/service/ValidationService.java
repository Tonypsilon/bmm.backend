package de.tonypsilon.bmm.backend.validation.service;

import com.google.common.base.CharMatcher;
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

    private final String[] invalidNameCharacters = {";", "@", "\\", "\"", "<", ">"};

    /**
     * Validate a date string to not be empty / blank and to contain only
     * alphanumerical symbols or . or -
     *
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
     *
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
     *
     * @param emailAddress the emailAddress to be validated.
     */
    public void validateEmailAddress(@Nullable String emailAddress) {
        if(emailAddress == null || !emailValidator.isValid(emailAddress)) {
            throw new BadDataException("Die E-Mailadresse ist ungültig!");
        }
    }

    /**
     * Validates a phone number string to optionally start with a plus sign,
     * followed by only digits.
     *
     * @param phoneNumber the phone number to be validated.
     */
    public void validatePhoneNumber(String phoneNumber) {
        if (phoneNumber == null || phoneNumber.isBlank()) {
            throw new BadDataException("Die Telefonnummer darf nicht leer sein!");
        }
        if(!phoneNumber.matches("\\+?[\\d]+")) {
            throw new BadDataException("Die Telefonnummer enthält ungültige Zeichen!");
        }
    }

    /**
     * Validate a rating (like DWZ or ELO) to not be null and to be a
     * positive integer.
     *
     * @param rating the rating to be validated
     */
    public void validateRating(Integer rating) {
        if(rating == null) {
            throw new BadDataException("Das Rating darf nicht leer sein!");
        }
        if (rating <= 0) {
            throw new BadDataException("Das Rating muss positiv sein!");
        }
    }

    public void validateNoSpecialCharactersAndLength(String string, int length) {
        if (string.length() > length) {
            throw new BadDataException("Die Zeichenkette ist zu lang!");
        }
        if(string.matches("a-zA-Z0-9äöüÄÖÜ_éÉèÈáÁàÀ\\.-")) {
            throw new BadDataException("Die Zeichenkette enthält ungültige Zeichen!");
        }
    }

    public void validateAscii(String string, String messageStarter) {
        if (!CharMatcher.ascii().matchesAllOf(string)) {
            throw new BadDataException(messageStarter + " enthält ungültige Zeichen!");
        }
    }
}
