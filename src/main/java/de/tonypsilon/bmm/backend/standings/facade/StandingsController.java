package de.tonypsilon.bmm.backend.standings.facade;

import de.tonypsilon.bmm.backend.standings.data.StandingsData;
import de.tonypsilon.bmm.backend.standings.service.StandingsAssembler;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RestController;

import java.util.Objects;

@RestController
public class StandingsController {

    private final StandingsAssembler standingsAssembler;

    public StandingsController(
            final StandingsAssembler standingsAssembler
    ) {
        this.standingsAssembler = standingsAssembler;
    }

    @GetMapping(path = "/divisions/{divisionId}/standings", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<StandingsData> getStandingsForDivision(@PathVariable final Long divisionId) {
        return ResponseEntity
                .ok()
                .body(standingsAssembler.assembleStandings(Objects.requireNonNull(divisionId)));
    }

    @GetMapping(path = "/seasons/{seasonName}/divisions/{divisionName}/textual-standings", produces = MediaType.TEXT_PLAIN_VALUE)
    public String getTextualStandingsForDivision(@PathVariable final String seasonName, @PathVariable final String divisionName) {
        return standingsAssembler.assembleTextualStandings(
                Objects.requireNonNull(seasonName),
                Objects.requireNonNull(divisionName));
    }

}
