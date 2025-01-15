package yeogi.moim.gathering.service;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import yeogi.moim.authentication.service.AuthenticationService;
import yeogi.moim.gathering.dto.GatheringRequest;
import yeogi.moim.gathering.dto.GatheringResponse;
import yeogi.moim.gathering.dto.SearchGatheringRequest;
import yeogi.moim.gathering.entity.Gathering;
import yeogi.moim.gathering.repository.GatheringRepository;

import java.util.List;
import java.util.stream.Collectors;

@Service
public class GatheringService {

    private final GatheringRepository gatheringRepository;
    private final AuthenticationService authenticationService;

    public GatheringService(GatheringRepository gatheringRepository, AuthenticationService authenticationService) {
        this.gatheringRepository = gatheringRepository;
        this.authenticationService = authenticationService;
    }

    @Transactional
    public GatheringResponse registerGatheringAsALeader(GatheringRequest gatheringRequest, Long gatheringOwnerId) {
        Gathering gathering = gatheringRequest.toEntity(gatheringOwnerId);

        gatheringRepository.save(gathering);

        return GatheringResponse.from(gathering);
    }

    @Transactional(readOnly = true)
    public GatheringResponse getGathering(Long id) {
        Gathering gathering = gatheringRepository.findById(id).orElseThrow(
                () -> new IllegalArgumentException("찾고자 하는 모임이 존재하지 않습니다.")
        );

        return GatheringResponse.from(gathering);
    }

    @Transactional
    public List<GatheringResponse> searchGatheringList(SearchGatheringRequest searchGatheringRequest) {
        return gatheringRepository.searchGatheringList(searchGatheringRequest).stream()
                .map(GatheringResponse::from)
                .collect(Collectors.toList());
    }

    @Transactional
    public GatheringResponse updateGathering(Long id, GatheringRequest gatheringRequest) {
        Gathering gathering = gatheringRepository.findById(id).orElseThrow(
                () -> new IllegalArgumentException("수정할 모임이 존재하지 않습니다.")
        );

        Long memberId = authenticationService.getAuthenticatedMemberId();
        Long gatheringOwnerId = gathering.getOwnerId();

        if (!memberId.equals(gatheringOwnerId)) {
            throw new SecurityException("모임을 수정할 권한이 없는 사용자입니다.");
        }

        gathering.update(
                gatheringRequest.getTitle(),
                gatheringRequest.getDescription(),
                gatheringRequest.getTotalPersonnel(),
                gatheringRequest.getCategory()
        );

        return GatheringResponse.from(gathering);
    }

    @Transactional
    public void joinGatheringForMember(Long gatheringId) {
        Gathering gathering = gatheringRepository.findById(gatheringId).orElseThrow(
                () -> new IllegalArgumentException("모임이 존재하지 않습니다")
        );

        if (gathering.getTotalPersonnel() <= gathering.getCurrentPersonnel()) {
            throw new IllegalArgumentException("인원 초과로 인해 해당 모임에는 더 이상 참여할 수 없습니다.");
        }

        gathering.incrementCurrentPersonnel();
    }

    @Transactional
    public void deleteGathering(Long id) {
        gatheringRepository.deleteById(id);
    }
}
