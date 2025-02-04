package yeogi.moim.favorite.dto;

import lombok.Builder;
import lombok.Getter;
import yeogi.moim.favorite.entity.Favorite;

@Getter
public class FavoriteResponse {
    private Long id;
    private Long userId;
    private Long gatheringId;
    private Boolean isFavorite;
    private String gatheringTitle;
    private String gatheringDescription;

    @Builder
    public FavoriteResponse(Long id, Long userId, Long gatheringId, Boolean isFavorite, String gatheringTitle, String gatheringDescription) {
        this.id = id;
        this.userId = userId;
        this.gatheringId = gatheringId;
        this.isFavorite = isFavorite;
        this.gatheringTitle = gatheringTitle;
        this.gatheringDescription = gatheringDescription;
    }

    public static FavoriteResponse fromFavoriteGathering(Favorite favorite, String title, String description) {
        return FavoriteResponse.builder()
                .id(favorite.getId())
                .userId(favorite.getUserId())
                .gatheringId(favorite.getGatheringId())
                .isFavorite(favorite.getIsFavorite())
                .gatheringTitle(title)
                .gatheringDescription(description)
                .build();
    }

}
