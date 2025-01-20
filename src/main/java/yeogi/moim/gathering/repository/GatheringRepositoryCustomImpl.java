package yeogi.moim.gathering.repository;

import com.querydsl.core.types.OrderSpecifier;
import com.querydsl.core.types.Projections;
import com.querydsl.core.types.dsl.BooleanExpression;
import com.querydsl.jpa.impl.JPAQueryFactory;
import jakarta.persistence.EntityManager;
import org.springframework.stereotype.Repository;
import yeogi.moim.favorite.entity.QFavorite;
import yeogi.moim.gathering.dto.SearchGatheringDto;
import yeogi.moim.gathering.dto.SearchGatheringRequest;
import yeogi.moim.gathering.entity.Category;
import yeogi.moim.review.entity.QReview;

import java.time.LocalDateTime;
import java.util.ArrayList;
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
    public List<SearchGatheringDto> searchGatheringList(SearchGatheringRequest searchGatheringRequest) {
        return queryFactory
                .select(Projections.constructor(
                        SearchGatheringDto.class,
                        gathering.id,
                        gathering.title,
                        gathering.currentPersonnel,
                        gathering.totalPersonnel
                ))
                .from(gathering)
                .leftJoin(favorite).on(favorite.gatheringId.eq(gathering.id))
                .leftJoin(review).on(review.gatheringId.eq(gathering.id))
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
                            ? favorite.count().lt(cursorFavorite)
                                    .or(favorite.count().eq(cursorFavorite)
                                            .and(gathering.id.lt(cursorId)))

                            : favorite.count().gt(cursorFavorite)
                                    .or(favorite.count().eq(cursorFavorite)
                                            .and(gathering.id.gt(cursorId)))
                    );

        } else if ("review".equals(sortBy)) {
            Long cursorReview = cursorCondition.getCursorReviewCount();

            return cursorReview == null ? null :
                    (
                            isDescending
                            ? review.count().lt(cursorReview)
                                    .or(review.count().eq(cursorReview)
                                            .and(gathering.id.lt(cursorId)))

                            : review.count().gt(cursorReview)
                                    .or(review.count().eq(cursorReview)
                                            .and(gathering.id.gt(cursorId)))
                    );

        } else if ("title".equals(sortBy)) {
            String cursorTitle = cursorCondition.getCursorTitle();

            return cursorTitle == null ? null :
                    (
                            isDescending
                            ? gathering.title.lt(cursorTitle)
                                    .or(gathering.title.eq(cursorTitle)
                                        .and(gathering.id.lt(cursorId)))

                            : gathering.title.gt(cursorTitle)
                                    .or(gathering.title.eq(cursorTitle)
                                        .and(gathering.id.gt(cursorId)))
                    );

        } else if ("personnel".equals(sortBy)) {
            Integer cursorPersonnel = cursorCondition.getCursorPersonnel();

            return cursorPersonnel == null ? null :
                    (
                            isDescending
                            ? gathering.currentPersonnel.lt(cursorPersonnel)
                                    .or(gathering.currentPersonnel.eq(cursorPersonnel)
                                        .and(gathering.id.lt(cursorId)))

                            : gathering.currentPersonnel.gt(cursorPersonnel)
                                    .or(gathering.currentPersonnel.eq(cursorPersonnel)
                                        .and(gathering.id.gt(cursorId)))
                    );
        } else if ("createdDate".equals(sortBy)) {
            LocalDateTime cursorCreatedDate = cursorCondition.getCursorCreatedDate();

            return cursorCreatedDate == null ? null :
                    (
                            isDescending
                            ? gathering.createdDate.lt(cursorCreatedDate)
                                    .or(gathering.createdDate.eq(cursorCreatedDate)
                                            .and(gathering.id.lt(cursorId)))

                            : gathering.createdDate.gt(cursorCreatedDate)
                                    .or(gathering.createdDate.eq(cursorCreatedDate)
                                            .and(gathering.id.gt(cursorId)))
                    );
        }

        return null;
    }

    private OrderSpecifier<?>[] orderByCondition(SearchGatheringRequest.SortCondition sortCondition) {
        List<OrderSpecifier<?>> orderSpecifiers = new ArrayList<>();

        if ("favorite".equals(sortCondition.getSortBy())) {
            orderSpecifiers.add(sortCondition.isDescending() ? QFavorite.favorite.count().desc() : QFavorite.favorite.count().asc());
            orderSpecifiers.add(sortCondition.isDescending() ? gathering.id.desc() : gathering.id.asc());
        }
        else if ("review".equals(sortCondition.getSortBy())) {
            orderSpecifiers.add(sortCondition.isDescending() ? QReview.review.count().desc() : QReview.review.count().asc());
            orderSpecifiers.add(sortCondition.isDescending() ? gathering.id.desc() : gathering.id.asc());
        }
        else if ("title".equals(sortCondition.getSortBy())) {
            orderSpecifiers.add(sortCondition.isDescending() ? gathering.title.desc() : gathering.title.asc());
            orderSpecifiers.add(sortCondition.isDescending() ? gathering.id.desc() : gathering.id.asc());
        }
        else if ("personnel".equals(sortCondition.getSortBy())) {
            orderSpecifiers.add(sortCondition.isDescending() ? gathering.currentPersonnel.desc() : gathering.currentPersonnel.asc());
            orderSpecifiers.add(sortCondition.isDescending() ? gathering.id.desc() : gathering.id.asc());
        }
        else if ("createdDate".equals(sortCondition.getSortBy())) {
            orderSpecifiers.add(sortCondition.isDescending() ? gathering.createdDate.desc() : gathering.createdDate.asc());
            orderSpecifiers.add(sortCondition.isDescending() ? gathering.id.desc() : gathering.id.asc());
        }
        else {
            orderSpecifiers.add(gathering.id.asc());
        }

        return orderSpecifiers.toArray(new OrderSpecifier<?>[0]);
    }

}
