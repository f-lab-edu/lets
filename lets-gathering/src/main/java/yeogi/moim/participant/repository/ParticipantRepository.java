package yeogi.moim.participant.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import yeogi.moim.participant.entity.Participant;

public interface ParticipantRepository extends JpaRepository<Participant, Long> {
    Participant findByMemberIdAndGatheringId(Long reviewerId, Long gatheringId);
}
