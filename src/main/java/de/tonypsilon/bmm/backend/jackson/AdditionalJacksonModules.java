package de.tonypsilon.bmm.backend.jackson;

import com.fasterxml.jackson.datatype.guava.GuavaModule;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class AdditionalJacksonModules {

    @Bean
    public GuavaModule getGuavaModule() {
        return new GuavaModule();
    }
}
