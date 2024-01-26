package de.tonypsilon.bmm.backend.season.service;

import de.tonypsilon.bmm.backend.exception.*;
import de.tonypsilon.bmm.backend.season.data.*;
import de.tonypsilon.bmm.backend.validation.service.ValidationService;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Service;

import javax.annotation.Nonnull;
import javax.transaction.Transactional;
import java.util.Set;

@Service
public class PlayingDateService {
	
	private final PlayingDateRepository playingDateRepository;
	private final SeasonService seasonService;
	private final ValidationService validationService;
	
	public PlayingDateService(final PlayingDateRepository playingDateRepository,
			final SeasonService seasonService,
			final ValidationService validationService) {
		this.playingDateRepository = playingDateRepository;
		this.seasonService = seasonService;
		this.validationService = validationService;
	}
	
	@Transactional
	@Nonnull
	public PlayingDateData createPlayingDate(@Nonnull PlayingDateCreationData creationData) {
		if(!Set.of(SeasonStage.REGISTRATION, SeasonStage.PREPARATION)
				  .contains(seasonService.getStageOfSeason(creationData.seasonId()))) {
			throw new SeasonStageException("Die Saison ist nicht in der Registrierungs- oder Vorbereitungsphase!");
		}
		if(creationData.number() < 1) {
			throw new BadDataException("Die Runde muss eine positive Zahl sein!");
		}
		if(playingDateRepository.existsBySeasonIdAndNumber(creationData.seasonId(), creationData.number())) {
			throw new AlreadyExistsException("Es gibt bereits ein Datum für Saison %d und Runde %d!"
				.formatted(creationData.seasonId(), creationData.number()));
		}
		validationService.validateDateString(creationData.date());
		PlayingDate playingDate = new PlayingDate();
		playingDate.setSeasonId(creationData.seasonId());
		playingDate.setNumber(creationData.number());
		playingDate.setDate(creationData.date());
		playingDateRepository.save(playingDate);
		return playingDateToPlayingDateData(
				getPlayingDateBySeasonIdAndNumber(creationData.seasonId(), creationData.number()));
	}

	@NonNull
	public PlayingDateData getBySeasonIdAndNumber(@Nonnull Long seasonId, @Nonnull Integer number) {
		return playingDateToPlayingDateData(getPlayingDateBySeasonIdAndNumber(seasonId, number));
	}

	@Nonnull
	private PlayingDate getPlayingDateBySeasonIdAndNumber(@Nonnull Long seasonId, @Nonnull Integer number) {
		return playingDateRepository.findBySeasonIdAndNumber(seasonId, number)
				.orElseThrow(() -> new NotFoundException("Es gibt für Saison %d und Runde %d kein Datum!"
						.formatted(seasonId, number)));
	}
	
	@Nonnull
	private PlayingDateData playingDateToPlayingDateData(@Nonnull PlayingDate playingDate) {
		return new PlayingDateData(playingDate.getId(),
				playingDate.getSeasonId(),
				playingDate.getNumber(),
				playingDate.getDate());
	}

}
