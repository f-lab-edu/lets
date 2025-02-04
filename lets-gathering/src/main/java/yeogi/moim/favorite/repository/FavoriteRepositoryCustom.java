package yeogi.moim.favorite.repository;

import yeogi.moim.favorite.dto.FavoriteResponse;

import java.util.List;

public interface FavoriteRepositoryCustom {
    Long countFavoritesByGatheringId(Long gatheringId);
    List<FavoriteResponse> findByUserIdWithGathering(Long userId);
}
