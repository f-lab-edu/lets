package yeogi.moim.gathering.dto;

import lombok.Getter;
import yeogi.moim.gathering.entity.Category;

@Getter
public class SearchGatheringRequest {

    private FilterCondition filterCondition;
    private SortCondition sortCondition;
    private Integer offset;
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
}