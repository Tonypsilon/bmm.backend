package de.tonypsilon.bmm.backend.security.rnr.service;

import de.tonypsilon.bmm.backend.club.data.ClubData;
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

import java.util.Comparator;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

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
    @NonNull
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
    public Set<String> getAdminsOfClub(Long clubId) {
        return clubAdminRepository.findByClubId(clubId)
                .stream()
                .map(ClubAdmin::getUsername)
                .collect(Collectors.toSet());
    }

    @NonNull
    @Transactional
    public List<ClubData> getClubsOfClubAdmin(@NonNull String username) {
        return clubAdminRepository.findByUsername(username).stream()
                .map(ClubAdmin::getClubId)
                .map(clubService::getClubById)
                .sorted(Comparator.comparing(ClubData::name))
                .toList();
    }

    @NonNull
    public Boolean isClubAdmin(@NonNull Long clubId, @NonNull String username) {
        return clubAdminRepository.existsByClubIdAndUsername(clubId, username);
    }

    @Transactional
    public void deleteClubAdmin(@NonNull ClubAdminData clubAdminData) {
        ClubAdmin clubAdminToDelete = clubAdminRepository.findByClubIdAndUsername(
                clubAdminData.clubId(), clubAdminData.username())
                .orElseThrow(() -> new NotFoundException("Benutzer %s ist kein Administrator für den Verein mit ID %d!"
                        .formatted(clubAdminData.username(), clubAdminData.clubId())));
        clubAdminRepository.delete(clubAdminToDelete);
    }

    @NonNull
    private ClubAdminData clubAdminToClubAdminData(@NonNull ClubAdmin clubAdmin) {
        return new ClubAdminData(clubAdmin.getClubId(), clubAdmin.getUsername());
    }

}
