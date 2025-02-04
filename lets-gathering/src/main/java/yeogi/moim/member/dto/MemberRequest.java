package yeogi.moim.member.dto;

import lombok.Getter;
import org.springframework.security.crypto.password.PasswordEncoder;
import yeogi.moim.member.entity.Member;

@Getter
public class MemberRequest {
    private String username;
    private String email;
    private String password;

    public Member toEntity(PasswordEncoder passwordEncoder) {
        return new Member(
                email,
                username,
                passwordEncoder.encode(password)
        );
    }
}
