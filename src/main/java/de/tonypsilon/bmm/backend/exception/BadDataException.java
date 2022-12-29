package de.tonypsilon.bmm.backend.exception;

import java.io.Serial;

public class BadDataException extends BmmException {
	
	@Serial
    private static final long serialVersionUID = 1L;

    public BadDataException(String message) {
        super(message);
    }
}
