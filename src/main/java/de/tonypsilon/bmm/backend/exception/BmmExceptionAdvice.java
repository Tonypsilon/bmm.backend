package de.tonypsilon.bmm.backend.exception;

import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.servlet.mvc.method.annotation.ResponseEntityExceptionHandler;

@ControllerAdvice
public class BmmExceptionAdvice extends ResponseEntityExceptionHandler {

    @ResponseBody
    @ExceptionHandler(BmmException.class)
    ResponseEntity<ErrorData> bmmExceptionHandler(BmmException exception) {
        return ResponseEntity.badRequest().body(new ErrorData(exception.getMessage()));
    }

}
