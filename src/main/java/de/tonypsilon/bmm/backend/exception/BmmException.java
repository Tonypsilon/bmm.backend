package de.tonypsilon.bmm.backend.exception;

import java.io.Serial;

public class BmmException extends RuntimeException {

	@Serial
    private static final long serialVersionUID = 1L;

	public BmmException(String message) {
        super(message);
    }
}
