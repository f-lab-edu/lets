package yeogi.moim.participant.controller;

import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import yeogi.moim.participant.dto.ParticipantRequest;
import yeogi.moim.participant.dto.ParticipantResponse;
import yeogi.moim.participant.service.ParticipantService;

import java.util.List;

@RestController
@RequestMapping("/api/participants")
public class ParticipantController {

    private final ParticipantService participantService;

    public ParticipantController(ParticipantService participantService) {
        this.participantService = participantService;
    }

    @PostMapping
    public ParticipantResponse applyMemberParticipant(@RequestBody ParticipantRequest participantRequest) {
        return participantService.registerMemberParticipant(participantRequest);
    }

    @GetMapping
    public List<ParticipantResponse> getParticipantList() {
        return participantService.getParticipantList();
    }

    @GetMapping("/{id}")
    public ParticipantResponse getParticipant(@PathVariable Long id) {
        return participantService.getParticipant(id);
    }

    @DeleteMapping("/{id}")
    public void deleteParticipant(@PathVariable Long id) {
        participantService.deleteParticipant(id);
    }
}
