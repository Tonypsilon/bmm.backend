package de.tonypsilon.bmm.backend.participant.service;

import de.tonypsilon.bmm.backend.participant.data.ParticipantRepository;
import de.tonypsilon.bmm.backend.participationeligibility.service.ParticipationEligibilityService;
import de.tonypsilon.bmm.backend.season.service.SeasonService;
import de.tonypsilon.bmm.backend.team.service.TeamService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.mock;

class ParticipantServiceTest {

    private final ParticipantRepository participantRepository= mock(ParticipantRepository.class);
    private final TeamService teamService = mock(TeamService.class);
    private final ParticipationEligibilityService participationEligibilityService = mock(
            ParticipationEligibilityService.class);
    private final SeasonService seasonService = mock(SeasonService.class);
    private ParticipantService participantService;

    @BeforeEach
    private void setUp() {
        this.participantService = new ParticipantService(participantRepository,
                teamService,
                participationEligibilityService,
                seasonService);
    }

    @Test
    void testCreateValidParticipantConfigurationForTeamOk() {

    }

    @Test
    void testCreateValidParticipantConfigurationForTeamTeamDoesNotExist() {

    }

    @Test
    void testCreateValidParticipantConfigurationForTeamInvalidNumbers() {

    }

    @Test
    void testCreateValidParticipantConfigurationForTeamWrongSeasonStage() {

    }

    @Test
    void addParticipantToTeamOk() {

    }

    @Test
    void testAddParticipantToTeamParticipationEligibilityDoesNotExist() {

    }

    @Test
    void testAddParticipantToTeamParticipantForParticipationEligibilityAlreadyExists() {

    }

    @Test
    void testAddParticipantToTeamNumberNotAvailableForTeam() {

    }

    @Test
    void testAddParticipantToTeamWrongSeasonStage() {

    }

    @Test
    void testAddParticipantToTeamInvalidNumber() {

    }

    @Test
    void testDeleteParticipantOk() {

    }

    @Test
    void testDeleteParticipantThatDoesNotExist() {

    }

    @Test
    void testDeleteParticipantWrongSeasonStage() {

    }

    @Test
    void testDeleteParticipantWrongNumber() {

    }


}