package yeogi.moim.review.dto;

import lombok.Getter;
import yeogi.moim.review.entity.Review;

@Getter
public class ReviewRequest {
    private Long gatheringId;
    private Long reviewerId;
    private String content;

    public Review toEntity() {
        return new Review(
                gatheringId,
                reviewerId,
                content
        );
    }
}
