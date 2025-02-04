package yeogi.moim.favorite.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import yeogi.moim.favorite.entity.Favorite;

import java.util.Optional;

public interface FavoriteRepository extends JpaRepository<Favorite, Long>, FavoriteRepositoryCustom {
    Optional<Favorite> findByUserIdAndGatheringId(Long userId, Long gatheringId);
}
