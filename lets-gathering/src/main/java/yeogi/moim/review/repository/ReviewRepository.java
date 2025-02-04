package yeogi.moim.review.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import yeogi.moim.review.entity.Review;

import java.util.List;

public interface ReviewRepository extends JpaRepository<Review, Long> {
    List<Review> findByGatheringId(Long gatheringId);
}
