package de.tonypsilon.bmm.backend.organization.data;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.Set;

@Repository
public interface OrganizationMemberRepository extends JpaRepository<OrganizationMember, Long> {

    Set<OrganizationMember> findByOrganizationIdIn(Set<Long> organizationIds);

    Set<OrganizationMember> findByOrganization(Organization organization);
}
