package de.tonypsilon.bmm.backend.season.pairings;

import de.tonypsilon.bmm.backend.exception.BmmException;

import java.util.List;

public class PairingTable {

    private final List<Round> rounds;

    public static final PairingTable STANDARD = PairingTableBuilder
            .newBuilder()
            .withNumberOfParticipants(10)
            .withRound(Round.ofNumbers(1,10, 2,9, 3,8, 4,7, 5,6))
            .withRound(Round.ofNumbers(10,6, 7,5, 8,4, 9,3, 1,2))
            .withRound(Round.ofNumbers(2,10, 3,1, 4,9, 5,8, 6,7))
            .withRound(Round.ofNumbers(10,7, 8,6, 9,5, 1,4, 2,3))
            .withRound(Round.ofNumbers(3,10, 4,2, 5,1, 6,9, 7,8))
            .withRound(Round.ofNumbers(10,8, 9,7, 1,6, 2,5, 3,4))
            .withRound(Round.ofNumbers(4,10, 5,3, 6,2, 7,1, 8,9))
            .withRound(Round.ofNumbers(10,9, 1,8, 2,7, 3,6, 4,5))
            .withRound(Round.ofNumbers(5,10, 6,4, 7,3, 8,2, 9,1))
            .build();

    public static final PairingTable STANDARD_WITH_FIRST_ROUND_AS_LAST = PairingTableBuilder
            .newBuilder()
            .withNumberOfParticipants(10)
            .withRound(Round.ofNumbers(10,6, 7,5, 8,4, 9,3, 1,2))
            .withRound(Round.ofNumbers(2,10, 3,1, 4,9, 5,8, 6,7))
            .withRound(Round.ofNumbers(10,7, 8,6, 9,5, 1,4, 2,3))
            .withRound(Round.ofNumbers(3,10, 4,2, 5,1, 6,9, 7,8))
            .withRound(Round.ofNumbers(10,8, 9,7, 1,6, 2,5, 3,4))
            .withRound(Round.ofNumbers(4,10, 5,3, 6,2, 7,1, 8,9))
            .withRound(Round.ofNumbers(10,9, 1,8, 2,7, 3,6, 4,5))
            .withRound(Round.ofNumbers(5,10, 6,4, 7,3, 8,2, 9,1))
            .withRound(Round.ofNumbers(1,10, 2,9, 3,8, 4,7, 5,6))
            .build();

    PairingTable(final int numberOfParticipants, final List<Round> rounds) {
        if(numberOfParticipants != 10) {
            throw new BmmException("Es werden aktuell nur 10er Staffeln unterstützt!");
        }
        if(rounds.size() != numberOfParticipants - 1) {
            throw new BmmException("Falsche Anzahl Runden!");
        }
        this.rounds = rounds;
    }

    public List<Pairing> getPairingsOfRound(int round) {
        if (round < 1 || round > rounds.size()) {
            throw new BmmException("Diese Runde gibt es nicht!");
        }
        return this.rounds.get(round-1).pairings();
    }

}
