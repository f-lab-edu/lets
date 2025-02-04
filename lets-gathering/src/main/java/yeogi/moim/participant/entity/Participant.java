package yeogi.moim.participant.entity;


import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import yeogi.moim.common.BaseEntity;

@Getter
@Entity
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class Participant extends BaseEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id")
    private Long id;

    @Column(name = "member_id")
    private Long memberId;

    @Column(name = "gathering_id")
    private Long gatheringId;

    @Enumerated(EnumType.STRING)
    private Role role;

    @Column(name = "rating")
    private Double rating;

    public Participant(Long memberId, Long gatheringId, Role role, Double rating) {
        this.memberId = memberId;
        this.gatheringId = gatheringId;
        this.role = role;
        this.rating = rating;
    }

    public static Participant ofLeader(Long memberId, Long gatheringId) {
        return new Participant(memberId, gatheringId, Role.LEADER, 0.0);
    }
}
