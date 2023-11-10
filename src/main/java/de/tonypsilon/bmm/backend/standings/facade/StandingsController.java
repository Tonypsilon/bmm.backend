package de.tonypsilon.bmm.backend.standings.facade;

import de.tonypsilon.bmm.backend.standings.data.StandingsData;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RestController;

@RestController
public class StandingsController {

    @GetMapping(path = "/divisions/{divisionId}/standings", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<StandingsData> getStandingsForDivision(@PathVariable final Long divisionId) {
        return null;
    }

}
