package de.tonypsilon.bmm.backend.security.rnr.service;

import de.tonypsilon.bmm.backend.exception.AlreadyExistsException;
import de.tonypsilon.bmm.backend.exception.NotFoundException;
import de.tonypsilon.bmm.backend.season.service.SeasonService;
import de.tonypsilon.bmm.backend.security.rnr.data.SeasonAdmin;
import de.tonypsilon.bmm.backend.security.rnr.data.SeasonAdminData;
import de.tonypsilon.bmm.backend.security.rnr.data.SeasonAdminRepository;
import org.springframework.lang.NonNull;
import org.springframework.security.provisioning.UserDetailsManager;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
public class SeasonAdminService {

    private final SeasonAdminRepository seasonAdminRepository;
    private final SeasonService seasonService;
    private final UserDetailsManager userDetailsManager;

    public SeasonAdminService(final SeasonAdminRepository seasonAdminRepository,
                              final SeasonService seasonService,
                              final UserDetailsManager userDetailsManager) {
        this.seasonAdminRepository = seasonAdminRepository;
        this.seasonService = seasonService;
        this.userDetailsManager = userDetailsManager;
    }

    @Transactional
    public SeasonAdminData createSeasonAdmin(Long seasonId, String username) {
        if(!userDetailsManager.userExists(username)) {
            throw new NotFoundException("Es gibt keinen Benutzer mit dem Namen %s!".formatted(username));
        }
        if(!seasonService.seasonExistsById(seasonId)) {
            throw new NotFoundException("Es gibt keine Saison mit der ID %d!".formatted(seasonId));
        }
        if (seasonAdminRepository.existsBySeasonIdAndUsername(seasonId, username)) {
            throw new AlreadyExistsException("Benutzer %s ist bereits Administrator für die Saison mit ID %d!"
                    .formatted(username, seasonId));
        }
        SeasonAdmin seasonAdmin = new SeasonAdmin();
        seasonAdmin.setSeasonId(seasonId);
        seasonAdmin.setUsername(username);
        seasonAdminRepository.save(seasonAdmin);
        return seasonAdminToSeasonAdminData(seasonAdminRepository.getBySeasonIdAndUsername(seasonId,username));
    }

    public Boolean isSeasonAdmin(Long seasonId, String username) {
        return seasonAdminRepository.existsBySeasonIdAndUsername(seasonId, username);
    }

    @Transactional
    public void deleteSeasonAdmin(Long seasonId, String username) {
        SeasonAdmin seasonAdminToDelete = seasonAdminRepository.findBySeasonIdAndUsername(seasonId, username)
                        .orElseThrow(() -> new NotFoundException(
                                "Benutzer %s ist kein Administrator für die Saison mit ID %d"
                                        .formatted(username, seasonId)));
        seasonAdminRepository.delete(seasonAdminToDelete);
    }

    @NonNull
    private SeasonAdminData seasonAdminToSeasonAdminData(@NonNull SeasonAdmin seasonAdmin) {
        return new SeasonAdminData(seasonAdmin.getSeasonId(), seasonAdmin.getUsername());
    }
}
