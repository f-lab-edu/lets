package yeogi.moim.authentication.service;

import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;
import yeogi.moim.security.UserDetailsImpl;

@Service
public class AuthenticationService {

    public Long getAuthenticatedMemberId() {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        UserDetailsImpl userDetails = (UserDetailsImpl) authentication.getPrincipal();

        return userDetails.getMemberId();
    }

    public void authorizeMember(Long memberId) {
        Long authenticatedMemberId = getAuthenticatedMemberId();

        if (!authenticatedMemberId.equals(memberId)) {
            throw new IllegalArgumentException("입력받은 정보가 인증된 사용자와 일치하지 않습니다.");
        }
    }
}
