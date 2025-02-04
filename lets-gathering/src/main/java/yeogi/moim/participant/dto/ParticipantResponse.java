package yeogi.moim.participant.dto;

import lombok.Builder;
import lombok.Getter;
import yeogi.moim.participant.entity.Participant;
import yeogi.moim.participant.entity.Role;

@Getter
public class ParticipantResponse {
    private Long id;
    private Long memberId;
    private Long gatheringId;
    private Role role;
    private Double rating;

    @Builder
    public ParticipantResponse(Long id, Long memberId, Long gatheringId, Role role, Double rating) {
        this.id = id;
        this.memberId = memberId;
        this.gatheringId = gatheringId;
        this.role = role;
        this.rating = rating;
    }

    public static ParticipantResponse from(Participant participant) {
        return ParticipantResponse.builder()
                .id(participant.getId())
                .memberId(participant.getMemberId())
                .gatheringId(participant.getGatheringId())
                .role(participant.getRole())
                .rating(participant.getRating())
                .build();
    }
}
