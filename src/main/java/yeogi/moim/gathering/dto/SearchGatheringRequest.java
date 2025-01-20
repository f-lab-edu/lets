package yeogi.moim.gathering.dto;

import lombok.Getter;
import yeogi.moim.gathering.entity.Category;

import java.time.LocalDateTime;

@Getter
public class SearchGatheringRequest {

    private FilterCondition filterCondition;
    private SortCondition sortCondition;
    private CursorCondition cursorCondition;
    private Integer pageSize;

    @Getter
    public static class FilterCondition {
        private Category category;
        private Boolean available;
    }

    @Getter
    public static class SortCondition {
        private String sortBy;
        private boolean descending;
    }

    @Getter
    public static class CursorCondition {
        private String cursorTitle;
        private Long cursorFavoriteCount;
        private Long cursorReviewCount;
        private Integer cursorPersonnel;
        private LocalDateTime cursorCreatedDate;
        private Long cursorId;
    }
}