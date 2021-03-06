package de.tonypsilon.bmm.backend.security.rnr.service;

import de.tonypsilon.bmm.backend.club.service.ClubService;
import de.tonypsilon.bmm.backend.exception.AlreadyExistsException;
import de.tonypsilon.bmm.backend.exception.NotFoundException;
import de.tonypsilon.bmm.backend.security.rnr.data.ClubAdmin;
import de.tonypsilon.bmm.backend.security.rnr.data.ClubAdminData;
import de.tonypsilon.bmm.backend.security.rnr.data.ClubAdminRepository;
import org.springframework.lang.NonNull;
import org.springframework.security.provisioning.UserDetailsManager;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
public class ClubAdminService {

    private final ClubAdminRepository clubAdminRepository;
    private final ClubService clubService;
    private final UserDetailsManager userDetailsManager;

    public ClubAdminService(final ClubAdminRepository clubAdminRepository,
                            final ClubService clubService,
                            final UserDetailsManager userDetailsManager) {
        this.clubAdminRepository = clubAdminRepository;
        this.clubService = clubService;
        this.userDetailsManager = userDetailsManager;
    }

    @Transactional
    public ClubAdminData createClubAdmin(ClubAdminData clubAdminCreateData) {
        if(!userDetailsManager.userExists(clubAdminCreateData.username())) {
            throw new NotFoundException("Es gibt keinen Benutzer mit dem Namen %s!".formatted(clubAdminCreateData.username()));
        }
        if(!clubService.clubExistsById(clubAdminCreateData.clubId())) {
            throw new NotFoundException("Es gibt keinen Verein mit der ID %d!".formatted(clubAdminCreateData.clubId()));
        }
        if(clubAdminRepository.existsByClubIdAndUsername(clubAdminCreateData.clubId(), clubAdminCreateData.username())) {
            throw new AlreadyExistsException("Benutzer %s ist bereits Administrator für den Verein mit ID %d!"
                    .formatted(clubAdminCreateData.username(), clubAdminCreateData.clubId()));
        }
        ClubAdmin clubAdmin = new ClubAdmin();
        clubAdmin.setClubId(clubAdminCreateData.clubId());
        clubAdmin.setUsername(clubAdminCreateData.username());
        clubAdminRepository.save(clubAdmin);
        return clubAdminToClubAdminData(
                clubAdminRepository.getByClubIdAndUsername(clubAdminCreateData.clubId(), clubAdminCreateData.username()));
    }

    @NonNull
    public Boolean isClubAdmin(@NonNull Long clubId, @NonNull String username) {
        return clubAdminRepository.existsByClubIdAndUsername(clubId, username);
    }

    @Transactional
    public void deleteClubAdmin(Long clubId, String username) {
        ClubAdmin clubAdminToDelete = clubAdminRepository.findByClubIdAndUsername(clubId, username)
                .orElseThrow(() -> new NotFoundException("Benutzer %s ist kein Administrator für den Verein mit ID %d!"
                        .formatted(username, clubId)));
        clubAdminRepository.delete(clubAdminToDelete);
    }

    @NonNull
    private ClubAdminData clubAdminToClubAdminData(@NonNull ClubAdmin clubAdmin) {
        return new ClubAdminData(clubAdmin.getClubId(), clubAdmin.getUsername());
    }
}
