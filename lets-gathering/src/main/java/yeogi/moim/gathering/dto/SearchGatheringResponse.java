package yeogi.moim.gathering.dto;

import lombok.Builder;
import lombok.Getter;

@Getter
public class SearchGatheringResponse {

    private Long id;
    private String title;
    private Integer totalPersonnel;
    private Integer currentPersonnel;

    @Builder
    public SearchGatheringResponse(Long id, String title, Integer totalPersonnel, Integer currentPersonnel) {
        this.id = id;
        this.title = title;
        this.totalPersonnel = totalPersonnel;
        this.currentPersonnel = currentPersonnel;
    }

    public static SearchGatheringResponse from(SearchGatheringDto searchGatheringDto) {
        return SearchGatheringResponse.builder()
                .id(searchGatheringDto.getId())
                .title(searchGatheringDto.getTitle())
                .totalPersonnel(searchGatheringDto.getTotalPersonnel())
                .currentPersonnel(searchGatheringDto.getCurrentPersonnel())
                .build();
    }
}
