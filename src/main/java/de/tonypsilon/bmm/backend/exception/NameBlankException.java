package de.tonypsilon.bmm.backend.exception;


import java.io.Serial;

public class NameBlankException extends BmmException {
	
	@Serial
    private static final long serialVersionUID = 1L;

    public NameBlankException(String message) {
        super(message);
    }
}
