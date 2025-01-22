package org.apache.tools.ant.taskdefs.optional.ccm;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import org.junit.Test;

public class CCMCheckinDiffblueTest {
  /**
   * Test new {@link CCMCheckin} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link CCMCheckin}
   */
  @Test
  public void testNewCCMCheckin() {
    // Arrange and Act
    CCMCheckin actualCcmCheckin = new CCMCheckin();

    // Assert
    assertEquals("ccm", actualCcmCheckin.getCcmCommand());
    assertNull(actualCcmCheckin.getFile());
    assertNull(actualCcmCheckin.getDescription());
    assertNull(actualCcmCheckin.getTaskName());
    assertNull(actualCcmCheckin.getTaskType());
    assertNull(actualCcmCheckin.getTask());
    assertNull(actualCcmCheckin.getProject());
    assertNull(actualCcmCheckin.getOwningTarget());
    assertTrue(actualCcmCheckin.filesets.isEmpty());
    assertEquals(Continuus.COMMAND_CHECKIN, actualCcmCheckin.getCcmAction());
  }
}
