package yeogi.moim.review.repository;

import com.querydsl.jpa.impl.JPAQueryFactory;
import jakarta.persistence.EntityManager;
import org.springframework.stereotype.Repository;
import yeogi.moim.gathering.dto.GatheringReviewRequest;
import yeogi.moim.review.entity.QReview;
import yeogi.moim.review.entity.Review;

import java.util.List;

@Repository
public class ReviewRepositoryCustomImpl implements ReviewRepositoryCustom {

    private final JPAQueryFactory queryFactory;

    public ReviewRepositoryCustomImpl(EntityManager em) {
        this.queryFactory = new JPAQueryFactory(em);
    }

    @Override
    public List<Review> findReviewsByGatheringId(GatheringReviewRequest gatheringReviewRequest) {
        return queryFactory
                .selectFrom(QReview.review)
                .where(QReview.review.gatheringId.eq(gatheringReviewRequest.getGatheringId()),
                        QReview.review.id.gt(gatheringReviewRequest.getReviewId())
                )
                .limit(gatheringReviewRequest.getPageSize())
                .fetch();
    }
}
