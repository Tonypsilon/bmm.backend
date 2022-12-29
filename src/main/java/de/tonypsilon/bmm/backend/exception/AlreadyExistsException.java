package de.tonypsilon.bmm.backend.exception;

import java.io.Serial;

public class AlreadyExistsException extends BmmException {

	@Serial
    private static final long serialVersionUID = 1L;

	public AlreadyExistsException(String message) {
        super(message);
    }
}
