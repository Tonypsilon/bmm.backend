package de.tonypsilon.bmm.backend.season.pairings;

import java.util.List;

public record Round(List<Pairing> pairings) {

    public static Round ofNumbers(int home1, int away1,
                                  int home2, int away2,
                                  int home3, int away3,
                                  int home4, int away4,
                                  int home5, int away5) {
        return new Round(List.of(
                new Pairing(home1, away1),
                new Pairing(home2, away2),
                new Pairing(home3, away3),
                new Pairing(home4, away4),
                new Pairing(home5, away5)
        ));
    }

    public static Round ofNumbers(int home1, int away1,
                                  int home2, int away2,
                                  int home3, int away3,
                                  int home4, int away4) {
        return new Round(List.of(
                new Pairing(home1, away1),
                new Pairing(home2, away2),
                new Pairing(home3, away3),
                new Pairing(home4, away4)
        ));
    }
}
