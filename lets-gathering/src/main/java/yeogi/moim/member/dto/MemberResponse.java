package yeogi.moim.member.dto;

import lombok.Builder;
import lombok.Getter;
import yeogi.moim.member.entity.Member;

import java.time.LocalDateTime;

@Getter
public class MemberResponse {
    private Long id;
    private String username;
    private String email;
    private LocalDateTime createdDate;
    private LocalDateTime lastModifiedDate;

    @Builder
    public MemberResponse(Long id, String username, String email, LocalDateTime createdDate, LocalDateTime lastModifiedDate) {
        this.id = id;
        this.username = username;
        this.email = email;
        this.createdDate = createdDate;
        this.lastModifiedDate = lastModifiedDate;
    }

    public static MemberResponse from(Member member) {
        return MemberResponse.builder()
                .id(member.getId())
                .username(member.getUsername())
                .email(member.getEmail())
                .createdDate(member.getCreatedDate())
                .lastModifiedDate(member.getLastModifiedDate())
                .build();
    }
}
