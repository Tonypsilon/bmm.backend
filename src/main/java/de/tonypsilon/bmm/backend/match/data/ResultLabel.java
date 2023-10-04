package de.tonypsilon.bmm.backend.match.data;

import de.tonypsilon.bmm.backend.game.service.Result;

public enum ResultLabel {
    UNKNOWN("?:?"),
    WHITE_WIN("1:0"),
    BLACK_WIN("0:1"),
    DRAW("½:½"),
    WHITE_WIN_FORFEIT("+:-"),
    BLACK_WIN_FORFEIT("-:+"),
    BOTH_LOSE_FORFEIT("-:-");

    private final String label;

    ResultLabel(String label) {
        this.label = label;
    }

    public String getLabel() {
        return this.label;
    }
}
