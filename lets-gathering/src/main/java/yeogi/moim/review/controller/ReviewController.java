package yeogi.moim.review.controller;

import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import yeogi.moim.review.dto.ReviewRequest;
import yeogi.moim.review.dto.ReviewResponse;
import yeogi.moim.review.service.ReviewService;


@RestController
@RequestMapping("/api/reviews")
public class ReviewController {

    private final ReviewService reviewService;

    public ReviewController(ReviewService reviewService) {
        this.reviewService = reviewService;
    }

    @PostMapping
    public ReviewResponse registerReview(@RequestBody ReviewRequest reviewRequest) {
        return reviewService.registerReview(reviewRequest);
    }

    @GetMapping("/{id}")
    public ReviewResponse getReview(@PathVariable Long id) {
        return reviewService.getReview(id);
    }

    @PutMapping("/{id}")
    public ReviewResponse updateReview(@PathVariable Long id, @RequestBody ReviewRequest reviewRequest) {
        return reviewService.updateReview(id, reviewRequest);
    }

    @DeleteMapping("/{id}")
    public void deleteReview(@PathVariable Long id) {
        reviewService.deleteReview(id);
    }

}
