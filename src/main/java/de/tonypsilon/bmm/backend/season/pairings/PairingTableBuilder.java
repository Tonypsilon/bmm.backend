package de.tonypsilon.bmm.backend.season.pairings;

import java.util.ArrayList;
import java.util.List;

public class PairingTableBuilder {

    private int numberOfRounds;
    private List<Round> rounds;

    private PairingTableBuilder() {
        this.rounds = new ArrayList<>();
    }

    static PairingTableBuilder newBuilder() {
        return new PairingTableBuilder();
    }

    PairingTableBuilder withNumberOfRounds(int numberOfRounds) {
        this.numberOfRounds = numberOfRounds;
        return this;
    }

    PairingTableBuilder withRound(Round round) {
        this.rounds.add(round);
        return this;
    }

    PairingTable build() {
        return new PairingTable(this.numberOfRounds, this.rounds);
    }
}
