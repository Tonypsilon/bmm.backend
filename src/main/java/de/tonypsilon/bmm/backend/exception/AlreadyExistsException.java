package de.tonypsilon.bmm.backend.exception;

import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.ResponseStatus;

@ResponseStatus(value = HttpStatus.BAD_REQUEST)
public class AlreadyExistsException extends BmmException {

    public AlreadyExistsException(String message) {
        super(message);
    }
}
