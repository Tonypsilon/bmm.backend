package de.tonypsilon.bmm.backend.organization.service;

import de.tonypsilon.bmm.backend.club.service.ClubService;
import de.tonypsilon.bmm.backend.exception.*;
import de.tonypsilon.bmm.backend.organization.data.*;
import de.tonypsilon.bmm.backend.season.service.SeasonService;
import de.tonypsilon.bmm.backend.season.service.SeasonStage;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;

import java.util.Collections;
import java.util.Optional;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.*;

class OrganizationServiceTest {

    private final OrganizationRepository organizationRepository = mock(OrganizationRepository.class);
    private final OrganizationMemberRepository organizationMemberRepository =
            mock(OrganizationMemberRepository.class);
    private final SeasonService seasonService = mock(SeasonService.class);
    private final ClubService clubService = mock(ClubService.class);
    private OrganizationService organizationService;
    private Organization existingOrganization, newOrganization;
    private OrganizationMember existingOrganizationMember, newOrganizationMember;

    @BeforeEach
    void setUp() {
        organizationService = new OrganizationService(organizationRepository,
                organizationMemberRepository,
                seasonService,
                clubService);
        existingOrganizationMember = new OrganizationMember();
        existingOrganizationMember.setId(2L);
        existingOrganizationMember.setClubId(3L);
        existingOrganization = new Organization();
        existingOrganization.setId(10L);
        existingOrganization.setSeasonId(1L);
        existingOrganization.setName("cool club");
        existingOrganization.setOrganizationMembers(Set.of(existingOrganizationMember));
        existingOrganizationMember.setOrganization(existingOrganization);

        newOrganizationMember = new OrganizationMember();
        newOrganizationMember.setId(4L);
        newOrganizationMember.setClubId(2L);
        newOrganization = new Organization();
        newOrganization.setId(11L);
        newOrganization.setSeasonId(1L);
        newOrganization.setName("org name");
        newOrganization.setOrganizationMembers(Set.of(newOrganizationMember));
        newOrganizationMember.setOrganization(newOrganization);
    }

    @Test
    void testCreateOrganizationOk() {
        when(seasonService.seasonExistsById(1L)).thenReturn(Boolean.TRUE);
        when(seasonService.getStageOfSeason(1L)).thenReturn(SeasonStage.REGISTRATION);
        when(clubService.clubExistsById(2L)).thenReturn(Boolean.TRUE);
        when(clubService.clubExistsById(3L)).thenReturn(Boolean.TRUE);
        when(organizationRepository.findBySeasonId(1L)).thenReturn(Set.of(existingOrganization));
        when(organizationMemberRepository.findByOrganizationIdIn(eq(Set.of(10L))))
                .thenReturn(Set.of(existingOrganizationMember));

        OrganizationCreationData organizationCreationData =
                new OrganizationCreationData(1L, "org name", Set.of(2L));

        when(organizationRepository.getBySeasonIdAndName(1L, "org name"))
                .thenReturn(newOrganization);

        OrganizationData actual = organizationService.createOrganization(organizationCreationData);
        assertEquals(new OrganizationData(11L, 1L, "org name", Set.of(2L)),actual);

        verify(organizationRepository, times(1)).save(argThat(
                organization -> organization.getName().equals("org name")
                        && organization.getSeasonId().equals(1L)
                        && organization.getOrganizationMembers().size() == 1
        ));

        assertEquals(Set.of(2L), actual.clubIds());
    }

    @Test
    void testCreateOrganizationBadName() {
        OrganizationCreationData organizationCreationData =
                new OrganizationCreationData(1L, "", null);
        BadDataException actualException = assertThrows(BadDataException.class,
                () -> organizationService.createOrganization(organizationCreationData));
        assertEquals("Der Name der Organisation darf nicht leer sein!", actualException.getMessage());
    }

    @Test
    void testCreateOrganizationSeasonDoesNotExist() {
        when(seasonService.seasonExistsById(2L)).thenReturn(Boolean.FALSE);
        OrganizationCreationData organizationCreationData =
                new OrganizationCreationData(2L, "org name", null);
        NotFoundException actualException = assertThrows(NotFoundException.class,
                () -> organizationService.createOrganization(organizationCreationData));
        assertEquals("Es gibt keine Saison mit der ID 2!", actualException.getMessage());
    }

    @ParameterizedTest
    @EnumSource(value = SeasonStage.class, mode = EnumSource.Mode.EXCLUDE, names = {"REGISTRATION"})
    void testCreateOrganizationWrongSeasonStage(SeasonStage stage) {
        when(seasonService.seasonExistsById(1L)).thenReturn(Boolean.TRUE);
        when(seasonService.getStageOfSeason(1L)).thenReturn(stage);
        OrganizationCreationData organizationCreationData =
                new OrganizationCreationData(1L, "org name", null);
        SeasonStageException actualException = assertThrows(SeasonStageException.class,
                () -> organizationService.createOrganization(organizationCreationData));
        assertEquals("Saison ist nicht in der Registrierungsphase!", actualException.getMessage());
    }

    @Test
    void testCreateOrganizationNoClubsGiven() {
        when(seasonService.seasonExistsById(1L)).thenReturn(Boolean.TRUE);
        when(seasonService.getStageOfSeason(1L)).thenReturn(SeasonStage.REGISTRATION);

        OrganizationCreationData organizationCreationDataClubsNull =
                new OrganizationCreationData(1L, "org name", null);
        BadDataException actualExceptionClubsNull = assertThrows(BadDataException.class,
                () -> organizationService.createOrganization(organizationCreationDataClubsNull));
        assertEquals("Zur Erstellung einer Organisation muss mindestens ein Verein gegeben sein!",
                actualExceptionClubsNull.getMessage());

        OrganizationCreationData organizationCreationDataClubsEmpty =
                new OrganizationCreationData(1L, "org name", Collections.emptySet());
        BadDataException actualExceptionClubsEmpty = assertThrows(BadDataException.class,
                () -> organizationService.createOrganization(organizationCreationDataClubsEmpty));
        assertEquals("Zur Erstellung einer Organisation muss mindestens ein Verein gegeben sein!",
                actualExceptionClubsEmpty.getMessage());
    }

    @Test
    void testCreateOrganizationClubDoesNotExist() {
        OrganizationCreationData organizationCreationData =
                new OrganizationCreationData(1L, "org name", Set.of(1L, -1L));

        when(seasonService.seasonExistsById(1L)).thenReturn(Boolean.TRUE);
        when(seasonService.getStageOfSeason(1L)).thenReturn(SeasonStage.REGISTRATION);
        when(clubService.clubExistsById(1L)).thenReturn(Boolean.TRUE);
        when(clubService.clubExistsById(-1L)).thenReturn(Boolean.FALSE);

        NotFoundException actualException = assertThrows(NotFoundException.class,
                () -> organizationService.createOrganization(organizationCreationData));
        assertEquals("Es gibt keinen Verein mit der ID -1!", actualException.getMessage());
    }

    @Test
    void testCreateOrganizationClubAlreadyPartOfOrganization() {
        when(seasonService.seasonExistsById(1L)).thenReturn(Boolean.TRUE);
        when(seasonService.getStageOfSeason(1L)).thenReturn(SeasonStage.REGISTRATION);
        when(clubService.clubExistsById(3L)).thenReturn(Boolean.TRUE);
        when(organizationRepository.findBySeasonId(1L)).thenReturn(Set.of(existingOrganization));
        when(organizationMemberRepository.findByOrganizationIdIn(eq(Set.of(10L))))
                .thenReturn(Set.of(existingOrganizationMember));

        OrganizationCreationData organizationCreationData =
                new OrganizationCreationData(1L, "org name", Set.of(3L));

        AlreadyExistsException actualException = assertThrows(AlreadyExistsException.class,
                () -> organizationService.createOrganization(organizationCreationData));
        assertEquals("Es gibt schon eine Organisation in der Saison mit der ID 1 für den Verein mit der ID 3!",
                actualException.getMessage());
    }

    @Test
    void testGetSeasonIdOfOrganizationOk() {
        when(organizationRepository.findById(10L)).thenReturn(Optional.of(existingOrganization));
        Long actual = organizationService.getSeasonIdOfOrganization(10L);
        assertEquals(1L, actual);
    }

    @Test
    void testGetSeasonIdOfOrganizationNotFound() {
        when(organizationRepository.findById(-1L)).thenReturn(Optional.empty());
        NotFoundException actualException = assertThrows(NotFoundException.class,
                () -> organizationService.getSeasonIdOfOrganization(-1L));
        assertEquals("Es gibt keine Organisation mit der ID -1!", actualException.getMessage());
    }

}