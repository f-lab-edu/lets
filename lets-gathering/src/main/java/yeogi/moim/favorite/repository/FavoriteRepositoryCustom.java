package yeogi.moim.favorite.repository;

import yeogi.moim.favorite.dto.FavoriteResponse;
import yeogi.moim.favorite.dto.MemberWhoLikedGathering;

import java.util.List;

public interface FavoriteRepositoryCustom {
    Long countFavoritesByGatheringId(Long gatheringId);
    List<FavoriteResponse> findMyFavoriteGatheringsByUserId(Long userId);
    List<MemberWhoLikedGathering> findMembersWhoLikedGatheringsByGatheringId(Long gatheringId);
}
