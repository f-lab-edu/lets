package yeogi.moim.favorite.repository;

import com.querydsl.core.types.Projections;
import com.querydsl.jpa.impl.JPAQueryFactory;
import jakarta.persistence.EntityManager;
import org.springframework.stereotype.Repository;
import yeogi.moim.favorite.dto.FavoriteResponse;

import java.util.List;

import yeogi.moim.favorite.entity.QFavorite;
import yeogi.moim.gathering.entity.QGathering;

@Repository
public class FavoriteRepositoryCustomImpl implements FavoriteRepositoryCustom {

    private final JPAQueryFactory queryFactory;

    public FavoriteRepositoryCustomImpl(EntityManager em) {
        this.queryFactory = new JPAQueryFactory(em);
    }

    @Override
    public Long countFavoritesByGatheringId(Long gatheringId) {
        return queryFactory
                .select(QFavorite.favorite.count())
                .from(QFavorite.favorite)
                .where(
                        QFavorite.favorite.gatheringId.eq(gatheringId)
                                .and(QFavorite.favorite.isFavorite.isTrue())
                )
                .fetchOne();
    }

    @Override
    public List<FavoriteResponse> findMyFavoriteGatheringsByUserId(Long userId) {
        return queryFactory.select(Projections.constructor(FavoriteResponse.class,
                        QFavorite.favorite.id,
                        QFavorite.favorite.userId,
                        QFavorite.favorite.gatheringId,
                        QFavorite.favorite.isFavorite,
                        QGathering.gathering.title,
                        QGathering.gathering.description
                ))
                .from(QFavorite.favorite)
                .leftJoin(QGathering.gathering).on(QFavorite.favorite.gatheringId.eq(QGathering.gathering.id))
                .where(QFavorite.favorite.userId.eq(userId))
                .fetch();
    }
}
