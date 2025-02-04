package yeogi.moim.gathering.dto;

import lombok.Getter;

@Getter
public class SearchGatheringDto {
    private Long id;
    private String title;
    private Integer currentPersonnel;
    private Integer totalPersonnel;

    public SearchGatheringDto(Long id, String title, Integer currentPersonnel, Integer totalPersonnel) {
        this.id = id;
        this.title = title;
        this.currentPersonnel = currentPersonnel;
        this.totalPersonnel = totalPersonnel;
    }
}
