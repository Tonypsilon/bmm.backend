package de.tonypsilon.bmm.backend.participant.service;

import de.tonypsilon.bmm.backend.participant.data.ParticipantRepository;
import de.tonypsilon.bmm.backend.participationeligibility.service.ParticipationEligibilityService;
import de.tonypsilon.bmm.backend.team.service.TeamService;
import org.junit.jupiter.api.BeforeEach;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.mock;

class ParticipantServiceTest {

    private final ParticipantRepository participantRepository= mock(ParticipantRepository.class);
    private final TeamService teamService = mock(TeamService.class);
    private final ParticipationEligibilityService participationEligibilityService = mock(
            ParticipationEligibilityService.class);
    private ParticipantService participantService;

    @BeforeEach
    private void setUp() {
        this.participantService = new ParticipantService(participantRepository,
                teamService,
                participationEligibilityService);
    }


}