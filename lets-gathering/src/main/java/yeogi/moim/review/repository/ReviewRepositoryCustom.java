package yeogi.moim.review.repository;

import yeogi.moim.gathering.dto.GatheringReviewRequest;
import yeogi.moim.review.entity.Review;

import java.util.List;

public interface ReviewRepositoryCustom {
    List<Review> findReviewsByGatheringId(GatheringReviewRequest gatheringReviewRequest);
}
