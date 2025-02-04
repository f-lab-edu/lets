package yeogi.moim.gathering.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import yeogi.moim.common.BaseEntity;

@Entity
@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class Gathering extends BaseEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id")
    private Long id;

    @Column(name = "owner_id")
    private Long ownerId;

    @Column(name = "title", nullable = false)
    private String title;

    @Column(name = "description", nullable = false)
    private String description;

    @Column(name = "total_personnel", nullable = false)
    private Integer totalPersonnel;

    @Column(name = "current_personnel")
    private Integer currentPersonnel;

    @Enumerated(EnumType.STRING)
    private Category category;

    public Gathering(Long ownerId, String title, String description, Category category, Integer totalPersonnel, Integer currentPersonnel) {
        this.ownerId = ownerId;
        this.title = title;
        this.description = description;
        this.category = category;
        this.totalPersonnel = totalPersonnel;
        this.currentPersonnel = currentPersonnel;
    }

    public void update(String title, String description, Integer totalPersonnel, Category category) {
        this.title = title;
        this.description = description;
        this.totalPersonnel = totalPersonnel;
        this.category = category;
    }

    public void incrementCurrentPersonnel() {
        this.currentPersonnel += 1;
    }

    public void decrementCurrentPersonnel() {
        if (this.currentPersonnel > 1) {
            this.currentPersonnel -= 1;
        }
    }
}
