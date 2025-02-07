package yeogi.moim.gathering.controller;

import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import yeogi.moim.gathering.dto.GatheringReviewRequest;
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

    @PostMapping("/reviews")
    public List<ReviewResponse> getGatheringReviews(@RequestBody GatheringReviewRequest gatheringReviewRequest) {
        return getGatheringReviewService.getGatheringReviews(gatheringReviewRequest);
    }
}
