package de.tonypsilon.bmm.backend;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.context.properties.ConfigurationPropertiesScan;

@SpringBootApplication
@ConfigurationPropertiesScan("de.tonypsilon.bmm.backend")
public class BmmApplication {

	public static void main(String[] args) {
		SpringApplication.run(BmmApplication.class, args);
	}

}
