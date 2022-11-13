package de.tonypsilon.bmm.backend.season.data;

import de.tonypsilon.bmm.backend.season.service.SeasonStage;
import javax.persistence.*;
import org.springframework.lang.NonNull;

@Entity
public class Season {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(unique = true, nullable = false)
    private String name;

    @Column(unique = false, nullable = false)
    @Enumerated(EnumType.STRING)
    private SeasonStage stage;

    @NonNull
    public Long getId() {
        return id;
    }

    public void setId(@NonNull Long id) {
        this.id = id;
    }

    @NonNull
    public String getName() {
        return name;
    }

    public void setName(@NonNull String name) {
        this.name = name;
    }

    @NonNull
    public SeasonStage getStage() {
        return stage;
    }

    public void setStage(@NonNull SeasonStage stage) {
        this.stage = stage;
    }
}
