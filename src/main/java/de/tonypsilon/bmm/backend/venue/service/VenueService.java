package de.tonypsilon.bmm.backend.venue.service;

import de.tonypsilon.bmm.backend.club.service.ClubService;
import de.tonypsilon.bmm.backend.exception.AlreadyExistsException;
import de.tonypsilon.bmm.backend.exception.BadDataException;
import de.tonypsilon.bmm.backend.exception.NotFoundException;
import de.tonypsilon.bmm.backend.organization.service.OrganizationService;
import de.tonypsilon.bmm.backend.validation.service.ValidationService;
import de.tonypsilon.bmm.backend.venue.data.*;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

@Service
public class VenueService {

    private final VenueRepository venueRepository;
    private final ClubService clubService;
    private final ValidationService validationService;
    private final OrganizationService organizationService;

    public VenueService(final VenueRepository venueRepository,
                        final ClubService clubService,
                        final ValidationService validationService,
                        final OrganizationService organizationService) {
        this.venueRepository = venueRepository;
        this.clubService = clubService;
        this.validationService = validationService;
        this.organizationService = organizationService;
    }

    @Transactional
    @NonNull
    public VenueData createVenue(@NonNull VenueCreationData creationData) {
        if(creationData.clubId() == null) {
            throw new BadDataException("Es muss ein Verein gegeben sein!");
        }
        clubService.verifyClubExistsById(creationData.clubId());
        if(creationData.address() == null || creationData.address().isBlank()) {
            throw new BadDataException("Es muss eine Adresse gegeben sein!");
        }
        validateAddressAndHints(creationData.address(), creationData.hints());
        if(venueRepository.existsByClubIdAndAddress(creationData.clubId(), creationData.address())) {
            throw new AlreadyExistsException("Es gibt bereits einen Spielort unter der Adresse %s für den Verein %d!"
                    .formatted(creationData.address(), creationData.clubId()));
        }
        Venue venue = new Venue();
        venue.setClubId(creationData.clubId());
        venue.setAddress(creationData.address());
        Optional.ofNullable(creationData.hints()).ifPresent(venue::setHints);
        venueRepository.save(venue);

        return venueToVenueData(
                venueRepository.getByClubIdAndAddress(creationData.clubId(), creationData.address()));
    }

    /**
     * Method to change the address as well as the hints of a venue. The club may not change.
     * @param updateVenueData the updated venue data
     * @return the updated venue
     */
    @Transactional
    @NonNull
    public VenueData updateVenue(@NonNull VenueData updateVenueData) {
        Venue venueToBeUpdated = getById(updateVenueData.id());
        if (!venueToBeUpdated.getClubId().equals(updateVenueData.clubId())) {
            throw new BadDataException("Der Verein für einen Spielort darf sich nicht ändern!");
        }
        validateAddressAndHints(updateVenueData.address(), updateVenueData.hints());
        if(venueRepository.existsByClubIdAndAddress(updateVenueData.clubId(), updateVenueData.address())) {
            throw new AlreadyExistsException("Es gibt bereits einen Spielort unter der Adresse %s für den Verein %d!"
                    .formatted(updateVenueData.address(), updateVenueData.clubId()));
        }
        venueToBeUpdated.setAddress(updateVenueData.address());
        venueToBeUpdated.setHints(updateVenueData.hints());
        venueRepository.save(venueToBeUpdated);
        return venueToVenueData(
                venueRepository.getByClubIdAndAddress(updateVenueData.clubId(), updateVenueData.address()));
    }

    @Transactional
    @NonNull
    public List<VenueData> getVenuesForClub(@NonNull Long clubId) {
        return venueRepository.findByClubId(clubId)
                .stream()
                .map(this::venueToVenueData)
                .toList();
    }

    @Transactional
    @NonNull
    public List<VenueData> getVenuesForOrganization(@NonNull Long organizationId) {
        return organizationService.getOrganizationById(organizationId).clubIds()
                .stream()
                .map(this::getVenuesForClub)
                .flatMap(List::stream)
                .toList();
    }

    @Transactional
    @NonNull
    public List<VenueData> putVenuesForClub(@NonNull List<VenueData> venues) {
        List<VenueData> putVenues = new ArrayList<>();
        for(VenueData venue : venues) {
            if(venueRepository.existsByClubIdAndAddress(venue.clubId(), venue.address())) {
                putVenues.add(updateHints(venue));
            } else {
                putVenues.add(createVenue(new VenueCreationData(venue.clubId(), venue.address(), venue.hints())));
            }
        }
        return putVenues;
    }

    @NonNull
    private VenueData updateHints(VenueData venueData) {
        Venue venueToBeUpdated = venueRepository.findByClubIdAndAddress(venueData.clubId(), venueData.address())
                .orElseThrow(() -> new NotFoundException("Es gibt keine Adresse %s für Verein %d!"
                        .formatted(venueData.address(), venueData.clubId())));
        venueToBeUpdated.setHints(venueData.hints());
        venueRepository.save(venueToBeUpdated);
        return venueToVenueData(
                venueRepository.getByClubIdAndAddress(venueData.clubId(), venueData.address()));
    }

    public void verifyVenueExistsById(@NonNull Long venueId) {
        getById(venueId);
    }

    @NonNull
    public Long getClubIdByVenueId(@NonNull Long venueId) {
        return getById(venueId).getClubId();
    }

    @NonNull
    private Venue getById(@NonNull Long venueId) {
        return venueRepository.findById(venueId).orElseThrow(
                () -> new NotFoundException("Es gibt keinen Spielort mit ID %d!".formatted(venueId)));
    }

    @NonNull
    private VenueData venueToVenueData(@NonNull Venue venue) {
        return new VenueData(venue.getId(),
                venue.getClubId(),
                venue.getAddress(),
                venue.getHints().orElse(null));
    }

    private void validateAddressAndHints(String address, String hints) {
        validationService.validateNoSpecialCharactersAndLength(address, 128);
        Optional.ofNullable(hints).ifPresent(theHints -> {
            validationService.validateNoSpecialCharactersAndLength(theHints, 256);
        });
    }

}
