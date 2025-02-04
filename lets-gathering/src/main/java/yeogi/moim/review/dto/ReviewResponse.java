package yeogi.moim.review.dto;

import lombok.Builder;
import lombok.Getter;
import yeogi.moim.review.entity.Review;

@Getter
public class ReviewResponse {
    private Long id;
    private Long gatheringId;
    private Long reviewerId;
    private String content;

    @Builder
    ReviewResponse(Long id, Long gatheringId, Long reviewerId, String content) {
        this.id = id;
        this.gatheringId = gatheringId;
        this.reviewerId = reviewerId;
        this.content = content;
    }

    public static ReviewResponse from(Review review) {
        return ReviewResponse.builder()
                .id(review.getId())
                .gatheringId(review.getGatheringId())
                .reviewerId(review.getReviewerId())
                .content(review.getContent())
                .build();
    }
}
