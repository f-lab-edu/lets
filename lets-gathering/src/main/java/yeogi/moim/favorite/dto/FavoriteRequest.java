package yeogi.moim.favorite.dto;

import lombok.Getter;
import yeogi.moim.favorite.entity.Favorite;

@Getter
public class FavoriteRequest {
    private Long userId;
    private Long gatheringId;

    public Favorite toEntity() {
        return new Favorite(userId, gatheringId, true);
    }
}
