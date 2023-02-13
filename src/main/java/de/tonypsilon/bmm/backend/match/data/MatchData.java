package de.tonypsilon.bmm.backend.match.data;

import de.tonypsilon.bmm.backend.venue.data.VenueData;

import java.util.Optional;

public record MatchData(Long id,
                        Long matchdayId,
                        Long homeTeamId,
                        Long awayTeamId,
                        Optional<String> date,
                        Integer homeTeamPoints,
                        Integer awayTeamPoints,
                        Optional<Integer> overruledHomeBoardHalfPoints,
                        Optional<Integer> overruledAwayBoardHalfPoints,
                        Optional<Long> refereeId,
                        Boolean editable,
                        VenueData venueData) {
}
