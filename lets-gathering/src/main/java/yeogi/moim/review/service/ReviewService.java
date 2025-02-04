package yeogi.moim.review.service;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import yeogi.moim.authentication.service.AuthenticationService;
import yeogi.moim.gathering.dto.GatheringResponse;
import yeogi.moim.gathering.service.GatheringService;
import yeogi.moim.member.service.MemberService;
import yeogi.moim.participant.repository.ParticipantRepository;
import yeogi.moim.review.dto.ReviewRequest;
import yeogi.moim.review.dto.ReviewResponse;
import yeogi.moim.review.entity.Review;
import yeogi.moim.review.repository.ReviewRepository;


@Service
public class ReviewService {

    private final ReviewRepository reviewRepository;
    private final AuthenticationService authenticationService;
    private final GatheringService gatheringService;
    private final ParticipantRepository participantRepository;
    private final MemberService memberService;

    public ReviewService(ReviewRepository reviewRepository, AuthenticationService authenticationService, GatheringService gatheringService, ParticipantRepository participantRepository, MemberService memberService) {
        this.reviewRepository = reviewRepository;
        this.authenticationService = authenticationService;
        this.gatheringService = gatheringService;
        this.participantRepository = participantRepository;
        this.memberService = memberService;
    }

    @Transactional
    public ReviewResponse registerReview(ReviewRequest reviewRequest) {
        authenticationService.authorizeMember(reviewRequest.getReviewerId());
        GatheringResponse gatheringResponse = gatheringService.getGathering(reviewRequest.getGatheringId());

        Long reviewerId = reviewRequest.getReviewerId();
        Long gatheringId = reviewRequest.getGatheringId();

        if (gatheringResponse.getOwnerId().equals(reviewRequest.getReviewerId())) {
            throw new IllegalArgumentException("해당 모임의 리더는 후기를 작성할 수 없습니다.");
        }
        else if (participantRepository.findByMemberIdAndGatheringId(reviewerId, gatheringId) == null) {
            throw new IllegalArgumentException("참여하지 않은 모임에는 후기 작성이 불가합니다.");
        }

        Review review = reviewRequest.toEntity();

        reviewRepository.save(review);

        return ReviewResponse.from(review);
    }

    @Transactional(readOnly = true)
    public ReviewResponse getReview(Long id) {
        Review review = reviewRepository.findById(id).orElseThrow(
                () -> new IllegalArgumentException("해당 리뷰가 존재하지 않습니다.")
        );

        gatheringService.getGathering(review.getGatheringId());

        return ReviewResponse.from(review);
    }

    @Transactional
    public ReviewResponse updateReview(Long id, ReviewRequest reviewRequest) {
        authenticationService.authorizeMember(reviewRequest.getReviewerId());

        Review review = reviewRepository.findById(id).orElseThrow(
                () -> new IllegalArgumentException("해당 리뷰가 존재하지 않습니다.")
        );

        review.update(reviewRequest.getContent());

        return ReviewResponse.from(review);
    }

    @Transactional
    public void deleteReview(Long id) {
        Long memberId = authenticationService.getAuthenticatedMemberId();
        memberService.getMember(memberId);

        reviewRepository.findById(id).orElseThrow(
                () -> new IllegalArgumentException("해당 리뷰가 존재하지 않습니다")
        );

        reviewRepository.deleteById(id);
    }
}
