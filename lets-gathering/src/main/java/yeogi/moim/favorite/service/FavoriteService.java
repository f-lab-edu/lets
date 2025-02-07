package yeogi.moim.favorite.service;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import yeogi.moim.authentication.service.AuthenticationService;
import yeogi.moim.favorite.dto.MemberWhoLikedGathering;
import yeogi.moim.gathering.dto.GatheringResponse;
import yeogi.moim.gathering.service.GatheringService;
import yeogi.moim.favorite.dto.FavoriteRequest;
import yeogi.moim.favorite.dto.FavoriteResponse;
import yeogi.moim.favorite.entity.Favorite;
import yeogi.moim.favorite.repository.FavoriteRepository;
import yeogi.moim.member.service.MemberService;

import java.util.List;

@Service
public class FavoriteService {

    private final FavoriteRepository favoriteRepository;
    private final AuthenticationService authenticationService;
    private final GatheringService gatheringService;
    private final MemberService memberService;
    private final GetFavoriteCountService getFavoriteCountService;

    public FavoriteService(FavoriteRepository favoriteRepository, AuthenticationService authenticationService, GatheringService gatheringService, MemberService memberService, GetFavoriteCountService getFavoriteCountService) {
        this.favoriteRepository = favoriteRepository;
        this.authenticationService = authenticationService;
        this.gatheringService = gatheringService;
        this.memberService = memberService;
        this.getFavoriteCountService = getFavoriteCountService;
    }

    @Transactional
    public Long toggleFavorite(FavoriteRequest favoriteRequest) {
        Long userId = favoriteRequest.getUserId();
        Long gatheringId = favoriteRequest.getGatheringId();

        authenticationService.authorizeMember(userId);
        gatheringService.getGathering(gatheringId);

        return favoriteRepository.findByUserIdAndGatheringId(userId, gatheringId)
                .map(favorite -> {
                    favorite.toggleFavorite();
                    return getFavoriteCountService.getFavoriteCount(gatheringId);
                })

                .orElseGet(() -> {
                    registerFavorite(favoriteRequest);
                    return getFavoriteCountService.getFavoriteCount(gatheringId);
                });
    }

    @Transactional(readOnly = true)
    public List<FavoriteResponse> getFavoriteGatherings() {
        Long userId = authenticationService.getAuthenticatedMemberId();

        memberService.getMember(userId);

        return favoriteRepository.findMyFavoriteGatheringsByUserId(userId);
    }

    private void registerFavorite(FavoriteRequest favoriteRequest) {
        Favorite favorite = favoriteRequest.toEntity();

        favoriteRepository.save(favorite);
    }

    @Transactional(readOnly = true)
    public List<MemberWhoLikedGathering> getMembersWhoLikedGathering(Long id) {
        Long userId = authenticationService.getAuthenticatedMemberId();

        memberService.getMember(userId);
        GatheringResponse gatheringResponse = gatheringService.getGathering(id);

        if (!userId.equals(gatheringResponse.getOwnerId())) {
            throw new IllegalArgumentException("모임의 리더만 조회가 가능합니다.");
        }

        return favoriteRepository.findMembersWhoLikedGatheringsByGatheringId(id);
    }

}
