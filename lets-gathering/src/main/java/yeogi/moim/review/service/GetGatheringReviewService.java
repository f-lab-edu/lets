package yeogi.moim.review.service;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import yeogi.moim.authentication.service.AuthenticationService;
import yeogi.moim.gathering.service.GatheringService;
import yeogi.moim.member.service.MemberService;
import yeogi.moim.review.dto.ReviewResponse;
import yeogi.moim.review.repository.ReviewRepository;

import java.util.List;
import java.util.stream.Collectors;

@Service
public class GetGatheringReviewService {

    private final ReviewRepository reviewRepository;
    private final GatheringService gatheringService;
    private final AuthenticationService authenticationService;
    private final MemberService memberService;

    public GetGatheringReviewService(ReviewRepository repository, GatheringService gatheringService, AuthenticationService authenticationService, MemberService memberService) {
        this.reviewRepository = repository;
        this.gatheringService = gatheringService;
        this.authenticationService = authenticationService;
        this.memberService = memberService;
    }

    @Transactional(readOnly = true)
    public List<ReviewResponse> getGatheringReviewList(Long gatheringId) {
        Long memberId = authenticationService.getAuthenticatedMemberId();
        memberService.getMember(memberId);

        gatheringService.getGathering(gatheringId);

        return reviewRepository.findByGatheringId(gatheringId).stream()
                .map(ReviewResponse::from)
                .collect(Collectors.toList());
    }
}
