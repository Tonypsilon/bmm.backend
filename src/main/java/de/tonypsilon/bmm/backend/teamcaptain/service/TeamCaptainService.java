package de.tonypsilon.bmm.backend.teamcaptain.service;

import de.tonypsilon.bmm.backend.exception.AlreadyExistsException;
import de.tonypsilon.bmm.backend.exception.NotFoundException;
import de.tonypsilon.bmm.backend.team.service.TeamService;
import de.tonypsilon.bmm.backend.teamcaptain.data.*;
import de.tonypsilon.bmm.backend.validation.service.ValidationService;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
public class TeamCaptainService {

    private final TeamCaptainRepository teamCaptainRepository;
    private final TeamService teamService;
    private final ValidationService validationService;

    public TeamCaptainService(final TeamCaptainRepository teamCaptainRepository,
                              final TeamService teamService,
                              final ValidationService validationService) {
        this.teamCaptainRepository = teamCaptainRepository;
        this.teamService = teamService;
        this.validationService = validationService;
    }

    @Transactional
    @NonNull
    public TeamCaptainData createTeamCaptain(CreateTeamCaptainData createTeamCaptainData) {
        if(Boolean.FALSE.equals(teamService.existsById(createTeamCaptainData.teamId()))) {
            throw new NotFoundException("Es gibt keine Mannschaft mit der ID %d!"
                    .formatted(createTeamCaptainData.teamId()));
        }
        if(Boolean.TRUE.equals(teamCaptainRepository.existsByTeamId(createTeamCaptainData.teamId()))) {
            throw new AlreadyExistsException(
                    "Es gibt bereits einen Mannschaftsleiter für die Mannschaft mit der ID %d!"
                            .formatted(createTeamCaptainData.teamId()));
        }
        validationService.validateEmailAddress(createTeamCaptainData.emailAddress());
        validationService.validatePhoneNumber(createTeamCaptainData.phoneNumber());
        validationService.validateName(createTeamCaptainData.forename());
        validationService.validateName(createTeamCaptainData.surname());

        TeamCaptain teamCaptain = new TeamCaptain();
        teamCaptain.setTeamId(createTeamCaptainData.teamId());
        teamCaptain.setEmailAddress(createTeamCaptainData.emailAddress());
        teamCaptain.setPhoneNumber(createTeamCaptainData.phoneNumber());
        teamCaptain.setForename(createTeamCaptainData.forename());
        teamCaptain.setSurname(createTeamCaptainData.surname());

        teamCaptainRepository.save(teamCaptain);

        return teamCaptainToTeamCaptainData(
                teamCaptainRepository.getByTeamId(createTeamCaptainData.teamId()));
    }

    @NonNull
    private TeamCaptainData teamCaptainToTeamCaptainData(@NonNull TeamCaptain teamCaptain) {
        return new TeamCaptainData(teamCaptain.getId(),
                teamCaptain.getTeamId(),
                teamCaptain.getEmailAddress(),
                teamCaptain.getPhoneNumber(),
                teamCaptain.getForename(),
                teamCaptain.getSurname());
    }

}
