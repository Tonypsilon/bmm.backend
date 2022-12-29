package de.tonypsilon.bmm.backend.exception;

import java.io.Serial;

public class MissingDataException extends BmmException {
	
	@Serial
    private static final long serialVersionUID = 1L;

    public MissingDataException(String message) {
        super(message);
    }
}
