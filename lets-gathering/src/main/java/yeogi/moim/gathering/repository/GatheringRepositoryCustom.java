package yeogi.moim.gathering.repository;

import yeogi.moim.gathering.dto.SearchGatheringRequest;
import yeogi.moim.gathering.entity.Gathering;

import java.util.List;

public interface GatheringRepositoryCustom {
    List<Gathering> searchGatherings(SearchGatheringRequest searchGatheringRequest);
}
