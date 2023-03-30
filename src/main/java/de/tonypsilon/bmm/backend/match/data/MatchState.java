package de.tonypsilon.bmm.backend.match.data;

public enum MatchState {

    OPEN, CLOSED, IN_CLARIFICATION;

    public MatchState close() {
        return CLOSED;
    }

    public MatchState reopen() {
        return OPEN;
    }

    public MatchState clarificationNeeded() {
        return IN_CLARIFICATION;
    }
}
