package yeogi.moim.favorite.service;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import yeogi.moim.favorite.repository.FavoriteRepository;

@Service
public class GetFavoriteCountService {

    private final FavoriteRepository favoriteRepository;

    public GetFavoriteCountService(FavoriteRepository favoriteRepository) {
        this.favoriteRepository = favoriteRepository;
    }

    @Transactional(readOnly = true)
    public Long getFavoriteCount(Long gatheringId) {
        return favoriteRepository.countFavoritesByGatheringId(gatheringId);
    }
}
