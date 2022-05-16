package de.tonypsilon.bmm.backend.exception;

import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.ResponseStatus;

@ResponseStatus(value = HttpStatus.BAD_REQUEST)
public class NameBlankException extends BmmException {

    public NameBlankException(String message) {
        super(message);
        System.out.println("oh no, name blank");
    }
}
