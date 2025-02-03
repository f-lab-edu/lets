package yeogi.moim.gathering.repository;

import yeogi.moim.gathering.dto.SearchGatheringDto;
import yeogi.moim.gathering.dto.SearchGatheringRequest;

import java.util.List;

public interface GatheringRepositoryCustom {
    List<SearchGatheringDto> searchGatheringList(SearchGatheringRequest searchGatheringRequest);
}
