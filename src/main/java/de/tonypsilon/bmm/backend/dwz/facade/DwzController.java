package de.tonypsilon.bmm.backend.dwz.facade;

import de.tonypsilon.bmm.backend.dwz.service.DwzAssembler;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RestController;

import java.util.Objects;

@RestController
public class DwzController {

    private final DwzAssembler dwzAssembler;

    public DwzController(final DwzAssembler dwzAssembler) {
        this.dwzAssembler = dwzAssembler;
    }

    @GetMapping(
            value = "//seasons/{seasonName}/divisions/{divisionName}/dwz",
            produces = MediaType.TEXT_PLAIN_VALUE)
    public String getDwzForDivision(@PathVariable final String seasonName,
                                    @PathVariable final String divisionName) {
        return dwzAssembler.assembleDwz(
                Objects.requireNonNull(seasonName),
                Objects.requireNonNull(divisionName));
    }

}
