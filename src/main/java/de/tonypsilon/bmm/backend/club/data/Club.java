package de.tonypsilon.bmm.backend.club.data;

import org.springframework.lang.NonNull;

import javax.persistence.*;

@Entity
public class Club {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(unique = true, nullable = false)
    private String name;

    @Column(unique = true, nullable = false)
    private Integer zps;

    @Column(nullable = false)
    private Boolean active;

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
    public Integer getZps() {
        return zps;
    }

    public void setZps(@NonNull Integer zps) {
        this.zps = zps;
    }

    @NonNull
    public Boolean getActive() {
        return active;
    }

    public void setActive(@NonNull Boolean active) {
        this.active = active;
    }
}
