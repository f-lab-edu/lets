package yeogi.moim.member.controller;

import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import yeogi.moim.member.dto.MemberRequest;
import yeogi.moim.member.dto.MemberResponse;
import yeogi.moim.member.service.MemberService;

import java.util.List;

@RestController
@RequestMapping("/api/members")
@RequiredArgsConstructor
public class MemberController {

    private final MemberService memberService;

    @PostMapping
    public MemberResponse registerMember(@RequestBody MemberRequest memberRequest) {
        return memberService.registerMember(memberRequest);
    }

    @GetMapping
    public List<MemberResponse> getMemberList() {
        return memberService.getMemberList();
    }

    @GetMapping("/{id}")
    public MemberResponse getMember(@PathVariable Long id) {
        return memberService.getMember(id);
    }
}
