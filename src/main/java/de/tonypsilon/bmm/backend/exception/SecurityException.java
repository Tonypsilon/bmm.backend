package de.tonypsilon.bmm.backend.exception;

import java.io.Serial;

public class SecurityException extends BmmException {
	
	@Serial
    private static final long serialVersionUID = 1L;

    public SecurityException(String message) {
        super(message);
    }
}
