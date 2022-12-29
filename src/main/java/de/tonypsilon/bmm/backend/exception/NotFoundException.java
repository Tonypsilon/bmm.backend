package de.tonypsilon.bmm.backend.exception;

import java.io.Serial;

public class NotFoundException extends BmmException {
	
	@Serial
    private static final long serialVersionUID = 1L;

    public NotFoundException(String message) {
        super(message);
    }

}
