package yeogi.moim.gathering.dto;

import lombok.Builder;
import lombok.Getter;
import yeogi.moim.gathering.entity.Category;
import yeogi.moim.gathering.entity.Gathering;

@Getter
public class GatheringResponse {
    private Long id;
    private Long ownerId;
    private String title;
    private String description;
    private Category category;
    private Integer totalPersonnel;
    private Integer currentPersonnel;

    @Builder
    public GatheringResponse(Long id, Long ownerId, String title, Category category, String description, Integer totalPersonnel, Integer currentPersonnel) {
        this.id = id;
        this.ownerId = ownerId;
        this.title = title;
        this.description = description;
        this.category = category;
        this.totalPersonnel = totalPersonnel;
        this.currentPersonnel = currentPersonnel;
    }

    public static GatheringResponse from(Gathering gathering) {
        return GatheringResponse.builder()
                .id(gathering.getId())
                .ownerId(gathering.getOwnerId())
                .title(gathering.getTitle())
                .description(gathering.getDescription())
                .category(gathering.getCategory())
                .totalPersonnel(gathering.getTotalPersonnel())
                .currentPersonnel(gathering.getCurrentPersonnel())
                .build();
    }
}
