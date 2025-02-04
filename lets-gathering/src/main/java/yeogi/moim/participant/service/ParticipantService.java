package yeogi.moim.participant.service;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import yeogi.moim.authentication.service.AuthenticationService;
import yeogi.moim.gathering.dto.GatheringResponse;
import yeogi.moim.gathering.service.GatheringService;
import yeogi.moim.member.service.MemberService;
import yeogi.moim.participant.dto.ParticipantRequest;
import yeogi.moim.participant.dto.ParticipantResponse;
import yeogi.moim.participant.entity.Participant;
import yeogi.moim.participant.repository.ParticipantRepository;

import java.util.List;
import java.util.stream.Collectors;

@Service
public class ParticipantService {

    private final ParticipantRepository participantRepository;
    private final AuthenticationService authenticationService;
    private final MemberService memberService;
    private final GatheringService gatheringService;

    public ParticipantService(ParticipantRepository participantRepository, AuthenticationService authenticationService, MemberService memberService, GatheringService gatheringService) {
        this.participantRepository = participantRepository;
        this.authenticationService = authenticationService;
        this.memberService = memberService;
        this.gatheringService = gatheringService;
    }

    @Transactional
    public void registerLeaderParticipant(Long memberId, Long gatheringId) {
        memberService.getMember(memberId);
        gatheringService.getGathering(gatheringId);

        Participant participant = Participant.ofLeader(memberId, gatheringId);

        participantRepository.save(participant);
    }

    @Transactional
    public ParticipantResponse registerMemberParticipant(ParticipantRequest participantRequest) {
        GatheringResponse gatheringResponse = gatheringService.getGathering(participantRequest.getGatheringId());

        Long gatheringOwnerId = gatheringResponse.getOwnerId();
        Long memberId = authenticationService.getAuthenticatedMemberId();

        if (!memberId.equals(gatheringOwnerId)) {
            throw new IllegalArgumentException("모임의 리더만 해당 모임의 멤버를 승인할 수 있습니다.");
        }

        else if (memberId.equals(participantRequest.getMemberId())) {
            throw new IllegalArgumentException("가입 승인을 하려는 사용자와 모임에 참여하려는 사용자가 일치합니다.");
        }

        memberService.getMember(memberId);
        memberService.getMember(participantRequest.getMemberId());

        Participant participant = participantRequest.toEntity();
        participantRepository.save(participant);

        gatheringService.joinGatheringForMember(participantRequest.getGatheringId());

        return ParticipantResponse.from(participant);
    }

    @Transactional(readOnly = true)
    public List<ParticipantResponse> getParticipantList() {
        return participantRepository.findAll().stream()
                .map(ParticipantResponse::from)
                .collect(Collectors.toList());
    }

    @Transactional(readOnly = true)
    public ParticipantResponse getParticipant(Long id) {
        Participant participant = participantRepository.findById(id).orElseThrow(
                () -> new IllegalArgumentException("찾고자하는 참여자가 존재하지 않습니다.")
        );

        return ParticipantResponse.from(participant);
    }

    @Transactional
    public void deleteParticipant(Long id) {
        Participant participant = participantRepository.findById(id).orElseThrow(
                () -> new IllegalArgumentException("삭제하려는 참여자가 존재하지 않습니다.")
        );

        participantRepository.delete(participant);
    }
}
