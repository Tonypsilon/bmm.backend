package de.tonypsilon.bmm.backend.season.data;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;

import org.springframework.lang.NonNull;

@Entity
@Table(name = "playingdate")
public class PlayingDate {
	
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Long id;
	
	@Column(name = "season_id", nullable = false)
	private Long seasonId;
	
	@Column(nullable = false)
	private Integer number;
	
	@Column(nullable = false)
	private String date;

	@NonNull
	public Long getId() {
		return id;
	}

	public void setId(@NonNull Long id) {
		this.id = id;
	}

	@NonNull
	public Long getSeasonId() {
		return seasonId;
	}

	public void setSeasonId(@NonNull Long seasonId) {
		this.seasonId = seasonId;
	}

	@NonNull
	public Integer getNumber() {
		return number;
	}

	public void setNumber(@NonNull Integer number) {
		this.number = number;
	}

	@NonNull
	public String getDate() {
		return date;
	}

	public void setDate(@NonNull String date) {
		this.date = date;
	}
	
	
}
