package de.tonypsilon.bmm.backend.club.service;

import de.tonypsilon.bmm.backend.club.data.Club;
import de.tonypsilon.bmm.backend.club.data.ClubCreationData;
import de.tonypsilon.bmm.backend.club.data.ClubData;
import de.tonypsilon.bmm.backend.club.data.ClubRepository;
import de.tonypsilon.bmm.backend.exception.*;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Service;

import java.util.Collection;

@Service
public class ClubService {

    private final ClubRepository clubRepository;

    public ClubService(final ClubRepository clubRepository) {
        this.clubRepository = clubRepository;
    }

    public Collection<ClubData> getAllClubs() {
        return clubRepository.findAll().stream().map(this::clubToClubData).toList();
    }

    public ClubData createClub(@NonNull ClubCreationData clubCreationData) {
        checkIfClubNameIsValid(clubCreationData.name());
        checkThatClubWithNameDoesNotExist(clubCreationData.name());
        validateZps(clubCreationData.zps());
        checkThatClubWithZpsDoesNotExist(clubCreationData.zps());
        Club club = new Club();
        club.setName(clubCreationData.name());
        club.setZps(clubCreationData.zps());
        club.setActive(clubCreationData.active() == null ? true : clubCreationData.active());
        clubRepository.save(club);
        return clubToClubData(clubRepository.getByName(clubCreationData.name()));
    }

    public ClubData patchClub(@NonNull ClubData patchedClubData) {
        checkThatClubWithIdExists(patchedClubData.id());
        Club clubToBePatched = clubRepository.getById(patchedClubData.id());
        if(! clubToBePatched.getZps().equals(patchedClubData.zps())) {
            throw new BadPatchDataException("Die id und zps des Clubs stimmen nicht überein!");
        }
        clubToBePatched.setName(patchedClubData.name());
        clubToBePatched.setActive(patchedClubData.active());
        clubRepository.save(clubToBePatched);
        return clubToClubData(clubRepository.getById(patchedClubData.id()));
    }

    @NonNull
    private ClubData clubToClubData(@NonNull Club club) {
        return new ClubData(club.getId(), club.getName(), club.getZps(), club.getActive());
    }

    private void checkIfClubNameIsValid(String name) {
        if(name == null || name.isBlank()) {
            throw new NameBlankException("Der Name des Clubs darf nicht leer sein!");
        }
    }

    private void checkThatClubWithNameDoesNotExist(String name) {
        if(Boolean.TRUE.equals(clubRepository.existsByName(name))) {
            throw new AlreadyExistsException("Club mit dem Namen %s existiert bereits!".formatted(name));
        }
    }

    private void checkThatClubWithZpsDoesNotExist(@NonNull Integer zps) {
        if(Boolean.TRUE.equals(clubRepository.existsByZps(zps))) {
            throw new AlreadyExistsException("Club mit der zps %d existiert bereits!".formatted(zps));
        }
    }

    private void checkThatClubWithIdExists(Long id) {
        if(id == null || Boolean.FALSE.equals(clubRepository.existsById(id))) {
            throw new NotFoundException("Club mit der ID %d existiert nicht!".formatted(id));
        }
    }

    private void validateZps(Integer zps) {
        if(zps == null) {
            throw new MissingDataException("Club muss Eigenschaft zps besitzen!");
        }
    }
}
