package de.tonypsilon.bmm.backend.game.service;

public enum Result {
    WIN(2),
    DRAW(1),
    LOSS(0),
    WIN_FORFEIT(2),
    LOSS_FORFEIT(0);

    private final Integer doubledValue;

    Result(Integer doubledValue) {
        this.doubledValue = doubledValue;
    }

    public Integer getDoubledValue() {
        return doubledValue;
    }

    public String getLabel() {
        return switch (this) {
            case WIN -> "1";
            case DRAW -> "½";
            case LOSS -> "0";
            case WIN_FORFEIT -> "+";
            case LOSS_FORFEIT -> "-";
        };
    }
}
