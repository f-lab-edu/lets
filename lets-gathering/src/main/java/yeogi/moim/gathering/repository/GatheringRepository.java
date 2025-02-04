package yeogi.moim.gathering.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import yeogi.moim.gathering.entity.Gathering;

public interface GatheringRepository extends JpaRepository<Gathering, Long>, GatheringRepositoryCustom {
}
