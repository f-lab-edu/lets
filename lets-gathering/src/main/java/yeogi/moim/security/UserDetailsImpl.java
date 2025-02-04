package yeogi.moim.security;

import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.userdetails.UserDetails;
import yeogi.moim.member.entity.Member;

import java.util.ArrayList;
import java.util.Collection;

public class UserDetailsImpl implements UserDetails {
    private Member member;

    public UserDetailsImpl(Member member) {
        this.member = member;
    }

    public Long getMemberId() {
        return member.getId();
    }

    @Override
    public String getUsername() {
        return member.getEmail();
    }

    @Override
    public String getPassword() {
        return member.getPassword();
    }

    @Override
    public Collection<? extends GrantedAuthority> getAuthorities() {
        return new ArrayList<>(); // 권한 추가 가능
    }
}
