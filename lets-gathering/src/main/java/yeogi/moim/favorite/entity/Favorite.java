package yeogi.moim.favorite.entity;


import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Entity
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@Getter
public class Favorite {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(nullable = false)
    private Long userId;

    @Column(nullable = false)
    private Long gatheringId;

    @Column(nullable = false)
    private Boolean isFavorite;

    public Favorite(Long userId, Long gatheringId, Boolean isFavorite) {
        this.userId = userId;
        this.gatheringId = gatheringId;
        this.isFavorite = isFavorite;
    }

    public void toggleFavorite() {
        isFavorite = !isFavorite;
    }

}
