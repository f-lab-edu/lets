package yeogi.moim.gathering.dto;

import lombok.Getter;
import yeogi.moim.gathering.entity.Category;
import yeogi.moim.gathering.entity.Gathering;

@Getter
public class GatheringRequest {
    private String title;
    private String description;
    private Category category;
    private Integer totalPersonnel;

    public Gathering toEntity(Long ownerId) {
        return new Gathering(
                ownerId,
                title,
                description,
                category,
                totalPersonnel,
                1
        );
    }
}
