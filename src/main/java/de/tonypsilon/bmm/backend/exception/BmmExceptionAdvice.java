package de.tonypsilon.bmm.backend.exception;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.servlet.mvc.method.annotation.ResponseEntityExceptionHandler;

@ControllerAdvice
public class BmmExceptionAdvice extends ResponseEntityExceptionHandler {

    private final Logger logger = LoggerFactory.getLogger(BmmExceptionAdvice.class);

    @ResponseBody
    @ExceptionHandler(BmmException.class)
    ResponseEntity<ErrorData> bmmExceptionHandler(BmmException exception) {
        logger.warn(exception.getMessage(), exception);
        return ResponseEntity.badRequest().body(new ErrorData(exception.getMessage()));
    }

    @ResponseBody
    @ExceptionHandler(NullPointerException.class)
    ResponseEntity<ErrorData> nullPointerExceptionHandler(NullPointerException exception) {
        logger.warn(exception.getMessage(), exception);
        return ResponseEntity.badRequest().body(new ErrorData(exception.getMessage()));
    }

}
