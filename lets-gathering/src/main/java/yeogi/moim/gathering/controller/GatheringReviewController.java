package yeogi.moim.gathering.controller;

import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import yeogi.moim.review.dto.ReviewResponse;
import yeogi.moim.review.service.GetGatheringReviewService;

import java.util.List;

@RestController
@RequestMapping("/api/gatherings")
public class GatheringReviewController {

    private final GetGatheringReviewService getGatheringReviewService;

    public GatheringReviewController(GetGatheringReviewService getGatheringReviewService) {
        this.getGatheringReviewService = getGatheringReviewService;
    }

    @GetMapping("/{id}/reviews")
    public List<ReviewResponse> getGatheringReviews(@PathVariable Long id) {
        return getGatheringReviewService.getGatheringReviewList(id);
    }
}
