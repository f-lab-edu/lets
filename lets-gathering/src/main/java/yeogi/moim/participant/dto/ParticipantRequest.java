package yeogi.moim.participant.dto;

import lombok.Getter;
import yeogi.moim.participant.entity.Participant;
import yeogi.moim.participant.entity.Role;

@Getter
public class ParticipantRequest {

    private Long memberId;
    private Long gatheringId;

    public Participant toEntity() {
        return new Participant(
                memberId,
                gatheringId,
                Role.MEMBER,
                0.0
        );
    }
}
