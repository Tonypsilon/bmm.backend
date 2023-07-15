package de.tonypsilon.bmm.backend.security.rnr.service;

import de.tonypsilon.bmm.backend.exception.AlreadyExistsException;
import de.tonypsilon.bmm.backend.exception.NotFoundException;
import de.tonypsilon.bmm.backend.season.data.SeasonData;
import de.tonypsilon.bmm.backend.season.service.SeasonService;
import de.tonypsilon.bmm.backend.security.rnr.Role;
import de.tonypsilon.bmm.backend.security.rnr.data.SeasonAdmin;
import de.tonypsilon.bmm.backend.security.rnr.data.SeasonAdminData;
import de.tonypsilon.bmm.backend.security.rnr.data.SeasonAdminRepository;
import org.springframework.lang.NonNull;
import org.springframework.security.provisioning.UserDetailsManager;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Collection;
import java.util.Comparator;
import java.util.List;

@Service
public class SeasonAdminService {

    private final SeasonAdminRepository seasonAdminRepository;
    private final SeasonService seasonService;
    private final UserDetailsManager userDetailsManager;
    private final UserService userService;

    public SeasonAdminService(final SeasonAdminRepository seasonAdminRepository,
                              final SeasonService seasonService,
                              final UserDetailsManager userDetailsManager,
                              final UserService userService) {
        this.seasonAdminRepository = seasonAdminRepository;
        this.seasonService = seasonService;
        this.userDetailsManager = userDetailsManager;
        this.userService = userService;
    }

    @NonNull
    public Collection<SeasonAdminData> getAllSeasonAdmins() {
        return seasonAdminRepository.findAll()
                .stream()
                .map(this::seasonAdminToSeasonAdminData)
                .toList();
    }

    @Transactional
    public SeasonAdminData createSeasonAdmin(SeasonAdminData seasonAdminCreateData) {
        if(!userDetailsManager.userExists(seasonAdminCreateData.username())) {
            throw new NotFoundException("Es gibt keinen Benutzer mit dem Namen %s!".formatted(seasonAdminCreateData.username()));
        }
        if(!seasonService.seasonExistsById(seasonAdminCreateData.seasonId())) {
            throw new NotFoundException("Es gibt keine Saison mit der ID %d!".formatted(seasonAdminCreateData.seasonId()));
        }
        if(seasonAdminRepository.existsBySeasonIdAndUsername(seasonAdminCreateData.seasonId(), seasonAdminCreateData.username())) {
            throw new AlreadyExistsException("Benutzer %s ist bereits Administrator für die Saison mit ID %d!"
                    .formatted(seasonAdminCreateData.username(), seasonAdminCreateData.seasonId()));
        }
        userService.assignRoleToUser(seasonAdminCreateData.username(), Role.SEASON_ADMIN);
        SeasonAdmin seasonAdmin = new SeasonAdmin();
        seasonAdmin.setSeasonId(seasonAdminCreateData.seasonId());
        seasonAdmin.setUsername(seasonAdminCreateData.username());
        seasonAdminRepository.save(seasonAdmin);
        return seasonAdminToSeasonAdminData(
                seasonAdminRepository.getBySeasonIdAndUsername(
                        seasonAdminCreateData.seasonId(), seasonAdminCreateData.username()
                )
        );
    }

    @NonNull
    public Boolean isSeasonAdmin(@NonNull Long seasonId, @NonNull String username) {
        return seasonAdminRepository.existsBySeasonIdAndUsername(seasonId, username);
    }

    @NonNull
    public Boolean isSeasonAdmin(@NonNull String seasonName, @NonNull String username) {
        return seasonAdminRepository.existsBySeasonIdAndUsername(
                seasonService.getSeasonByName(seasonName).id(), username);
    }

    @Transactional
    public void deleteSeasonAdmin(@NonNull SeasonAdminData seasonAdminData) {
        SeasonAdmin seasonAdminToDelete = seasonAdminRepository.findBySeasonIdAndUsername(
                seasonAdminData.seasonId(), seasonAdminData.username())
                        .orElseThrow(() -> new NotFoundException(
                                "Benutzer %s ist kein Administrator für die Saison mit ID %d!"
                                        .formatted(seasonAdminData.username(), seasonAdminData.seasonId())));
        seasonAdminRepository.delete(seasonAdminToDelete);
    }

    @NonNull
    public List<SeasonData> getSeasonsOfSeasonAdmin(@NonNull String username) {
        return seasonAdminRepository.findByUsername(username).stream()
                .map(SeasonAdmin::getSeasonId)
                .map(seasonService::getSeasonById)
                .sorted(Comparator.comparing(SeasonData::name))
                .toList();
    }

    @NonNull
    private SeasonAdminData seasonAdminToSeasonAdminData(@NonNull SeasonAdmin seasonAdmin) {
        return new SeasonAdminData(seasonAdmin.getSeasonId(), seasonAdmin.getUsername());
    }
}
