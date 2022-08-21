package de.tonypsilon.bmm.backend.game.service;

public enum Result {
    WIN(2),
    DRAW(1),
    LOSS(0),
    WIN_FORFEIT(2),
    LOSS_FORFEIT(0);

    Result(Integer doubledValue) {
    }
}
