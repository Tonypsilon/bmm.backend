package de.tonypsilon.bmm.backend.exception;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.Serial;

public class BmmException extends RuntimeException {

    Logger logger = LoggerFactory.getLogger(BmmException.class);

	@Serial
    private static final long serialVersionUID = 1L;

	public BmmException(String message) {
        super(message);
        logger.warn(message);
    }
}
