package yeogi.moim.favorite.dto;

import lombok.Builder;
import lombok.Getter;

@Getter
public class MemberWhoLikedGathering {
    private Long memberId;

    @Builder
    public MemberWhoLikedGathering(Long memberId) {
        this.memberId = memberId;
    }

    public static MemberWhoLikedGathering from(Long memberId) {
        return MemberWhoLikedGathering.builder()
                .memberId(memberId)
                .build();
    }
}
