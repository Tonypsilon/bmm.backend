package de.tonypsilon.bmm.backend.club.service;

import de.tonypsilon.bmm.backend.club.data.Club;
import de.tonypsilon.bmm.backend.club.data.ClubCreationData;
import de.tonypsilon.bmm.backend.club.data.ClubData;
import de.tonypsilon.bmm.backend.club.data.ClubRepository;
import de.tonypsilon.bmm.backend.exception.AlreadyExistsException;
import de.tonypsilon.bmm.backend.exception.MissingDataException;
import de.tonypsilon.bmm.backend.exception.NameBlankException;
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
        if(clubCreationData.name() == null || clubCreationData.name().isBlank()) {
            throw new NameBlankException("Der Name des Clubs darf nicht leer sein!");
        }
        if(Boolean.TRUE.equals(clubRepository.existsByName(clubCreationData.name()))) {
            throw new AlreadyExistsException("Club mit dem Namen %s existiert bereits!".formatted(clubCreationData.name()));
        }
        if(clubCreationData.zps() == null) {
            throw new MissingDataException("Club muss Eigenschaft zps besitzen!");
        }
        if(Boolean.TRUE.equals(clubRepository.existsByZps(clubCreationData.zps()))) {
            throw new AlreadyExistsException("Club mit der zps %d existiert bereits!".formatted(clubCreationData.zps()));
        }
        Club club = new Club();
        club.setName(clubCreationData.name());
        club.setZps(clubCreationData.zps());
        club.setActive(clubCreationData.active() == null ? true : clubCreationData.active());
        clubRepository.save(club);
        return clubToClubData(clubRepository.getByName(clubCreationData.name()));
    }

    private ClubData clubToClubData(@NonNull Club club) {
        return new ClubData(club.getId(), club.getName(), club.getZps(), club.getActive());
    }
}
