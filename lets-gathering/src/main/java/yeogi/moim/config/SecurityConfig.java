package yeogi.moim.config;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configurers.AbstractHttpConfigurer;
import org.springframework.security.config.http.SessionCreationPolicy;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.security.web.SecurityFilterChain;
import yeogi.moim.security.UserDetailsServiceImpl;


@Configuration
public class SecurityConfig {

    private final UserDetailsServiceImpl userDetailsServiceImpl;

    public SecurityConfig(UserDetailsServiceImpl userDetailsServiceImpl) {
        this.userDetailsServiceImpl = userDetailsServiceImpl;
    }

    @Bean
    public SecurityFilterChain securityFilterChain(HttpSecurity http) throws Exception {
        http
                /*
                세션을 사용할 때에는 csrf 보호가 필요하기 때문에 csrf 보호 활성화해야 함. -> security 기본 설정
                .csrf(AbstractHttpConfigurer::disable)
                 */
                .csrf(AbstractHttpConfigurer::disable)
                .authorizeHttpRequests(authorize -> authorize
                        .requestMatchers("/", "/csrf", "/api/members", "/api/auth/signup").permitAll()
//                        .anyRequest().authenticated()
                        .anyRequest().permitAll()
                )
                .formLogin
                        (form -> form
                        .loginPage("/api/auth/login")
                        /*
                        인증이 필요한 url로 접속했을 때, 로그인 완료 후 처음 접속 시도한 url로 리디렉트한다.
                         */
                        .defaultSuccessUrl("/", false)
                        .permitAll()
                        .usernameParameter("email")
                )
                .logout(logout -> logout
                        .logoutUrl("/logout")
                        .logoutSuccessUrl("/")
                        .invalidateHttpSession(true)
                        .deleteCookies("JSESSIONID")
                        .permitAll()
                )
                .sessionManagement(session -> session
                        .sessionCreationPolicy(SessionCreationPolicy.IF_REQUIRED)
                        .maximumSessions(1) // 세션을 하나만 허용 -> 동시 접속 방지
                )
                .userDetailsService(userDetailsServiceImpl);

        return http.build();
    }

    @Bean
    public PasswordEncoder passwordEncoder() {
        return new BCryptPasswordEncoder();
    }
}