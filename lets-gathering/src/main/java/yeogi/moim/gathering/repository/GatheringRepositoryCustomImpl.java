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
import yeogi.moim.gathering.entity.QGathering;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;


@Repository
public class GatheringRepositoryCustomImpl implements GatheringRepositoryCustom {

    private final JPAQueryFactory queryFactory;

    public GatheringRepositoryCustomImpl(EntityManager em) {
        this.queryFactory = new JPAQueryFactory(em);
    }

    @Override
    public List<Gathering> searchGatherings(SearchGatheringRequest searchGatheringRequest) {
        return queryFactory
                .selectFrom(QGathering.gathering)
                .leftJoin(QFavorite.favorite).on(QFavorite.favorite.gatheringId.eq(QGathering.gathering.id))
                .leftJoin(QReview.review).on(QReview.review.gatheringId.eq(QGathering.gathering.id))
                .where(
                        categoryEq(searchGatheringRequest.getFilterCondition().getCategory()),
                        availableGathering(searchGatheringRequest.getFilterCondition().getAvailable()),
                        cursorCondition(searchGatheringRequest)
                )
                .orderBy(
                        orderByCondition(
                                searchGatheringRequest.getSortCondition()
                        )
                )
                .limit(searchGatheringRequest.getPageSize())
                .fetch();
    }

    private BooleanExpression categoryEq(Category category) {
        return category == null ? null : QGathering.gathering.category.eq(category);
    }

    private BooleanExpression availableGathering(Boolean available) {
        if (Boolean.TRUE.equals(available)) {
            return QGathering.gathering.totalPersonnel.ne(QGathering.gathering.currentPersonnel);
        }
        else if (Boolean.FALSE.equals(available)) {
            return QGathering.gathering.totalPersonnel.eq(QGathering.gathering.currentPersonnel);
        }
        return null;
    }

    private BooleanExpression cursorCondition(SearchGatheringRequest searchGatheringRequest) {
        SearchGatheringRequest.SortCondition sortCondition = searchGatheringRequest.getSortCondition();
        SearchGatheringRequest.CursorCondition cursorCondition = searchGatheringRequest.getCursorCondition();

        if (cursorCondition == null) {
            return null;
        }

        String sortBy = sortCondition.getSortBy();
        boolean isDescending = sortCondition.isDescending();
        Long cursorId = cursorCondition.getCursorId();

        if ("favorite".equals(sortBy)) {
            Long cursorFavorite = cursorCondition.getCursorFavoriteCount();

            return cursorFavorite == null ? null :
                    (
                            isDescending
                            ? QFavorite.favorite.count().lt(cursorFavorite)
                                    .or(QFavorite.favorite.count().eq(cursorFavorite)
                                            .and(QGathering.gathering.id.lt(cursorId)))

                            : QFavorite.favorite.count().gt(cursorFavorite)
                                    .or(QFavorite.favorite.count().eq(cursorFavorite)
                                            .and(QGathering.gathering.id.gt(cursorId)))
                    );

        } else if ("review".equals(sortBy)) {
            Long cursorReview = cursorCondition.getCursorReviewCount();

            return cursorReview == null ? null :
                    (
                            isDescending
                            ? QReview.review.count().lt(cursorReview)
                                    .or(QReview.review.count().eq(cursorReview)
                                            .and(QGathering.gathering.id.lt(cursorId)))

                            : QReview.review.count().gt(cursorReview)
                                    .or(QReview.review.count().eq(cursorReview)
                                            .and(QGathering.gathering.id.gt(cursorId)))
                    );

        } else if ("title".equals(sortBy)) {
            String cursorTitle = cursorCondition.getCursorTitle();

            return cursorTitle == null ? null :
                    (
                            isDescending
                            ? QGathering.gathering.title.lt(cursorTitle)
                                    .or(QGathering.gathering.title.eq(cursorTitle)
                                        .and(QGathering.gathering.id.lt(cursorId)))

                            : QGathering.gathering.title.gt(cursorTitle)
                                    .or(QGathering.gathering.title.eq(cursorTitle)
                                        .and(QGathering.gathering.id.gt(cursorId)))
                    );

        } else if ("personnel".equals(sortBy)) {
            Integer cursorPersonnel = cursorCondition.getCursorPersonnel();

            return cursorPersonnel == null ? null :
                    (
                            isDescending
                            ? QGathering.gathering.currentPersonnel.lt(cursorPersonnel)
                                    .or(QGathering.gathering.currentPersonnel.eq(cursorPersonnel)
                                        .and(QGathering.gathering.id.lt(cursorId)))

                            : QGathering.gathering.currentPersonnel.gt(cursorPersonnel)
                                    .or(QGathering.gathering.currentPersonnel.eq(cursorPersonnel)
                                        .and(QGathering.gathering.id.gt(cursorId)))
                    );
        } else if ("createdDate".equals(sortBy)) {
            LocalDateTime cursorCreatedDate = cursorCondition.getCursorCreatedDate();

            return cursorCreatedDate == null ? null :
                    (
                            isDescending
                            ? QGathering.gathering.createdDate.lt(cursorCreatedDate)
                                    .or(QGathering.gathering.createdDate.eq(cursorCreatedDate)
                                            .and(QGathering.gathering.id.lt(cursorId)))

                            : QGathering.gathering.createdDate.gt(cursorCreatedDate)
                                    .or(QGathering.gathering.createdDate.eq(cursorCreatedDate)
                                            .and(QGathering.gathering.id.gt(cursorId)))
                    );
        }

        return null;
    }

    private OrderSpecifier<?>[] orderByCondition(SearchGatheringRequest.SortCondition sortCondition) {
        List<OrderSpecifier<?>> orderSpecifiers = new ArrayList<>();

        if ("favorite".equals(sortCondition.getSortBy())) {
            orderSpecifiers.add(sortCondition.isDescending() ? QFavorite.favorite.count().desc() : QFavorite.favorite.count().asc());
            orderSpecifiers.add(sortCondition.isDescending() ? QGathering.gathering.id.desc() : QGathering.gathering.id.asc());
        }
        else if ("review".equals(sortCondition.getSortBy())) {
            orderSpecifiers.add(sortCondition.isDescending() ? QReview.review.count().desc() : QReview.review.count().asc());
            orderSpecifiers.add(sortCondition.isDescending() ? QGathering.gathering.id.desc() : QGathering.gathering.id.asc());
        }
        else if ("title".equals(sortCondition.getSortBy())) {
            orderSpecifiers.add(sortCondition.isDescending() ? QGathering.gathering.title.desc() : QGathering.gathering.title.asc());
            orderSpecifiers.add(sortCondition.isDescending() ? QGathering.gathering.id.desc() : QGathering.gathering.id.asc());
        }
        else if ("personnel".equals(sortCondition.getSortBy())) {
            orderSpecifiers.add(sortCondition.isDescending() ? QGathering.gathering.currentPersonnel.desc() : QGathering.gathering.currentPersonnel.asc());
            orderSpecifiers.add(sortCondition.isDescending() ? QGathering.gathering.id.desc() : QGathering.gathering.id.asc());
        }
        else if ("createdDate".equals(sortCondition.getSortBy())) {
            orderSpecifiers.add(sortCondition.isDescending() ? QGathering.gathering.createdDate.desc() : QGathering.gathering.createdDate.asc());
            orderSpecifiers.add(sortCondition.isDescending() ? QGathering.gathering.id.desc() : QGathering.gathering.id.asc());
        }
        else {
            orderSpecifiers.add(QGathering.gathering.id.asc());
        }

        return orderSpecifiers.toArray(new OrderSpecifier<?>[0]);
    }

}
