package org.apache.tools.ant.taskdefs.optional.ccm;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import org.junit.Test;

public class CCMCheckinDefaultDiffblueTest {
  /**
   * Test new {@link CCMCheckinDefault} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link CCMCheckinDefault}
   */
  @Test
  public void testNewCCMCheckinDefault() {
    // Arrange and Act
    CCMCheckinDefault actualCcmCheckinDefault = new CCMCheckinDefault();

    // Assert
    assertEquals("ccm", actualCcmCheckinDefault.getCcmCommand());
    assertNull(actualCcmCheckinDefault.getFile());
    assertNull(actualCcmCheckinDefault.getDescription());
    assertNull(actualCcmCheckinDefault.getTaskName());
    assertNull(actualCcmCheckinDefault.getTaskType());
    assertNull(actualCcmCheckinDefault.getComment());
    assertNull(actualCcmCheckinDefault.getProject());
    assertNull(actualCcmCheckinDefault.getOwningTarget());
    assertTrue(actualCcmCheckinDefault.filesets.isEmpty());
    assertEquals(CCMCheckinDefault.DEFAULT_TASK, actualCcmCheckinDefault.getTask());
    assertEquals(Continuus.COMMAND_CHECKIN, actualCcmCheckinDefault.getCcmAction());
  }
}
