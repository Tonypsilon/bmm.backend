package de.tonypsilon.bmm.backend.standings.facade;

import de.tonypsilon.bmm.backend.standings.data.ProgressChartData;
import de.tonypsilon.bmm.backend.standings.service.ProgressChartAssembler;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RestController;

import java.util.Objects;

@RestController
public class ProgressChartController {

    private final ProgressChartAssembler progressChartAssembler;

    public ProgressChartController(final ProgressChartAssembler progressChartAssembler) {
        this.progressChartAssembler = progressChartAssembler;
    }

    @GetMapping(value = "/divisions/{divisionId}/progress", produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ProgressChartData> getProgressChartForDivision(@PathVariable final Long divisionId) {
        return ResponseEntity
                .ok()
                .body(progressChartAssembler.assembleProgressChart(Objects.requireNonNull(divisionId)));
    }
}
