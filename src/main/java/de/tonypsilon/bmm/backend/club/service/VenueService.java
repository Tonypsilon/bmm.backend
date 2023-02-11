package de.tonypsilon.bmm.backend.club.service;

import de.tonypsilon.bmm.backend.club.data.*;
import de.tonypsilon.bmm.backend.exception.AlreadyExistsException;
import de.tonypsilon.bmm.backend.exception.BadDataException;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
public class VenueService {

    private final VenueRepository venueRepository;
    private final ClubService clubService;

    public VenueService(final VenueRepository venueRepository,
                        final ClubService clubService) {
        this.venueRepository = venueRepository;
        this.clubService = clubService;
    }

    @Transactional
    @NonNull
    public VenueData createVenue(@NonNull VenueCreationData creationData) {
        if(creationData.clubId() == null) {
            throw new BadDataException("Es muss ein Verein gegeben sein!");
        }
        if(!clubService.clubExistsById(creationData.clubId())) {
            throw new BadDataException("Es gibt keinen Verein mit der ID %d!"
                    .formatted(creationData.clubId()));
        }
        if(creationData.address() == null || creationData.address().isBlank()) {
            throw new BadDataException("Es muss eine Adresse gegeben sein!");
        }
        if(creationData.address().length() > 128) {
            throw new BadDataException("Die Adresse darf höchstens 128 Zeichen lang sein!");
        }
        creationData.hints().ifPresent(hints -> {
            if (hints.length() > 256) {
                throw new BadDataException("Die Adresshinweise dürfen höchstens 256 Zeichen lang sein!");
            }
        });
        if(venueRepository.existsByClubIdAndAddress(creationData.clubId(), creationData.address())) {
            throw new AlreadyExistsException("Es gibt bereits einen Spielort unter der Adresse %s für den Verein %d!"
                    .formatted(creationData.address(), creationData.clubId()));
        }
        Venue venue = new Venue();
        venue.setClubId(creationData.clubId());
        venue.setAddress(creationData.address());
        creationData.hints().ifPresent(venue::setHints);
        venueRepository.save(venue);

        return venueToVenueData(
                venueRepository.getByClubIdAndAddress(creationData.clubId(), creationData.address()));
    }

    @NonNull
    private VenueData venueToVenueData(@NonNull Venue venue) {
        return new VenueData(venue.getId(),
                venue.getClubId(),
                venue.getAddress(),
                venue.getHints());
    }

}