package de.tonypsilon.bmm.backend.organizationsetup.data;

import java.util.List;

public record TeamSetupData(Long organizationId,
                            Integer number,
                            Long venueId,
                            String name,
                            List<Long> participants) {
}