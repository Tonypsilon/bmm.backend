package de.tonypsilon.bmm.backend.exception;

import java.io.Serial;

public class SeasonStageException extends BmmException {
	
	@Serial
    private static final long serialVersionUID = 1L;

    public SeasonStageException(String message) {
        super(message);
    }
}
