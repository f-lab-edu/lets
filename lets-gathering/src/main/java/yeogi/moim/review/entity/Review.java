package yeogi.moim.review.entity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Entity(name = "review")
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@Getter
public class Review {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(nullable = false, name = "gathering_id")
    private Long gatheringId;

    @Column(nullable = false, name = "reviewer_id")
    private Long reviewerId;

    @Column
    private String content;

    public Review(Long gatheringId, Long reviewerId, String content) {
        this.gatheringId = gatheringId;
        this.reviewerId = reviewerId;
        this.content = content;
    }

    public void update(String content) {
        this.content = content;
    }
}
