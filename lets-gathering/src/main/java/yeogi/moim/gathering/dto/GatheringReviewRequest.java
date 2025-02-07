package yeogi.moim.gathering.dto;

import lombok.Getter;

@Getter
public class GatheringReviewRequest {
    private Long gatheringId;
    private Long reviewId;
    private Long pageSize;
}
