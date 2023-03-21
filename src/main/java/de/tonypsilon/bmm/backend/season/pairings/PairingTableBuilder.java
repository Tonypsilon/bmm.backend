package de.tonypsilon.bmm.backend.season.pairings;

import java.util.ArrayList;
import java.util.List;

public class PairingTableBuilder {

    private int numberOfParticipants;
    private List<Round> rounds;

    private PairingTableBuilder() {
        this.rounds = new ArrayList<>();
    }

    static PairingTableBuilder newBuilder() {
        return new PairingTableBuilder();
    }

    PairingTableBuilder withNumberOfParticipants(int numberOfParticipants) {
        this.numberOfParticipants = numberOfParticipants;
        return this;
    }

    PairingTableBuilder withRound(Round round) {
        this.rounds.add(round);
        return this;
    }

    PairingTable build() {
        return new PairingTable(this.numberOfParticipants, this.rounds);
    }
}
