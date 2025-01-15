package yeogi.moim.gathering.repository;

import com.querydsl.core.types.OrderSpecifier;
import com.querydsl.core.types.dsl.BooleanExpression;
import com.querydsl.jpa.impl.JPAQueryFactory;
import jakarta.persistence.EntityManager;
import org.springframework.stereotype.Repository;
import yeogi.moim.favorite.entity.QFavorite;
import yeogi.moim.gathering.dto.SearchGatheringRequest;
import yeogi.moim.gathering.entity.Category;
import yeogi.moim.gathering.entity.Gathering;
import yeogi.moim.review.entity.QReview;

import java.util.List;

import static yeogi.moim.favorite.entity.QFavorite.favorite;
import static yeogi.moim.gathering.entity.QGathering.gathering;
import static yeogi.moim.review.entity.QReview.review;

@Repository
public class GatheringRepositoryCustomImpl implements GatheringRepositoryCustom {

    private final JPAQueryFactory queryFactory;

    public GatheringRepositoryCustomImpl(EntityManager em) {
        this.queryFactory = new JPAQueryFactory(em);
    }

    @Override
    public List<Gathering> searchGatheringList(SearchGatheringRequest searchGatheringRequest) {
        return queryFactory
                .selectFrom(gathering)
                .leftJoin(favorite).on(favorite.gatheringId.eq(gathering.id))
                .leftJoin(review).on(review.gatheringId.eq(gathering.id))
                .where(
                        categoryEq(searchGatheringRequest.getFilterCondition().getCategory()),
                        availableGathering(searchGatheringRequest.getFilterCondition().getAvailable())
                )
                .groupBy(gathering.id)
                .orderBy(
                        orderByCondition(
                                searchGatheringRequest.getSortCondition()
                        )
                )
                .offset(searchGatheringRequest.getOffset())
                .limit(searchGatheringRequest.getPageSize())
                .fetch();
    }

    private BooleanExpression categoryEq(Category category) {
        return category == null ? null : gathering.category.eq(category);
    }

    private BooleanExpression availableGathering(Boolean available) {
        if (Boolean.TRUE.equals(available)) {
            return gathering.totalPersonnel.ne(gathering.currentPersonnel);
        }
        else if (Boolean.FALSE.equals(available)) {
            return gathering.totalPersonnel.eq(gathering.currentPersonnel);
        }
        return null;
    }

    private OrderSpecifier<?> orderByCondition(SearchGatheringRequest.SortCondition sortCondition) {
        if ("favorite".equals(sortCondition.getSortBy())) {
            return sortCondition.isDescending() ? QFavorite.favorite.count().desc() : QFavorite.favorite.count().asc();
        }
        else if ("review".equals(sortCondition.getSortBy())) {
            return sortCondition.isDescending() ? QReview.review.count().desc() : QReview.review.count().asc();
        }
        else if ("title".equals(sortCondition.getSortBy())) {
            return sortCondition.isDescending() ? gathering.title.desc() : gathering.title.asc();
        }
        else if ("personnel".equals(sortCondition.getSortBy())) {
            return sortCondition.isDescending() ? gathering.currentPersonnel.desc() : gathering.currentPersonnel.asc();
        }
        else if ("createdDate".equals(sortCondition.getSortBy())) {
            return sortCondition.isDescending() ? gathering.createdDate.desc() : gathering.createdDate.asc();
        }

        return gathering.createdDate.asc();
    }

}
