package yeogi.moim.gathering.controller;


import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import yeogi.moim.gathering.dto.GatheringRequest;
import yeogi.moim.gathering.dto.GatheringResponse;
import yeogi.moim.gathering.dto.SearchGatheringRequest;
import yeogi.moim.gathering.service.GatheringService;
import yeogi.moim.gathering.service.RegisterGatheringService;

import java.util.List;

@RestController
@RequestMapping("/api/gatherings")
public class GatheringController {

    private final GatheringService gatheringService;
    private final RegisterGatheringService registerGatheringService;

    public GatheringController(GatheringService gatheringService, RegisterGatheringService registerGatheringService) {
        this.gatheringService = gatheringService;
        this.registerGatheringService = registerGatheringService;
    }

    @PostMapping
    public GatheringResponse registerGathering(@RequestBody GatheringRequest gatheringRequest) {
        return registerGatheringService.registerGathering(gatheringRequest);
    }

    @GetMapping("/{id}")
    public GatheringResponse getGathering(@PathVariable Long id) {
        return gatheringService.getGathering(id);
    }

    @PostMapping("/search")
    public List<GatheringResponse> searchGatheringList(@RequestBody SearchGatheringRequest searchGatheringRequest) {
        return gatheringService.searchGatheringList(searchGatheringRequest);
    }

    @PutMapping("/{id}")
    public GatheringResponse updateGathering(@PathVariable Long id, @RequestBody GatheringRequest gatheringRequest) {
        return gatheringService.updateGathering(id, gatheringRequest);
    }

    @DeleteMapping("/{id}")
    public void deleteGathering(@PathVariable Long id) {
        gatheringService.deleteGathering(id);
    }
}
