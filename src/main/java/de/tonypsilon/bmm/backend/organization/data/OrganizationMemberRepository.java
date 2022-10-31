package de.tonypsilon.bmm.backend.organization.data;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.Set;

@Repository
public interface OrganizationMemberRepository extends JpaRepository<OrganizationMember, Long> {

    public Set<OrganizationMember> findByOrganizationIdIn(Set<Long> organizationIds);
}
