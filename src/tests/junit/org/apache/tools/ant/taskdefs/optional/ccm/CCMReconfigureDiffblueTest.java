package org.apache.tools.ant.taskdefs.optional.ccm;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import org.junit.Test;

public class CCMReconfigureDiffblueTest {
  /**
   * Test new {@link CCMReconfigure} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link CCMReconfigure}
   */
  @Test
  public void testNewCCMReconfigure() {
    // Arrange and Act
    CCMReconfigure actualCcmReconfigure = new CCMReconfigure();

    // Assert
    assertEquals("ccm", actualCcmReconfigure.getCcmCommand());
    assertNull(actualCcmReconfigure.getDescription());
    assertNull(actualCcmReconfigure.getTaskName());
    assertNull(actualCcmReconfigure.getTaskType());
    assertNull(actualCcmReconfigure.getCcmProject());
    assertNull(actualCcmReconfigure.getProject());
    assertNull(actualCcmReconfigure.getOwningTarget());
    assertFalse(actualCcmReconfigure.isRecurse());
    assertFalse(actualCcmReconfigure.isVerbose());
    assertEquals(Continuus.COMMAND_RECONFIGURE, actualCcmReconfigure.getCcmAction());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link CCMReconfigure#setCcmProject(String)}
   *   <li>{@link CCMReconfigure#setRecurse(boolean)}
   *   <li>{@link CCMReconfigure#setVerbose(boolean)}
   *   <li>{@link CCMReconfigure#getCcmProject()}
   *   <li>{@link CCMReconfigure#isRecurse()}
   *   <li>{@link CCMReconfigure#isVerbose()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    CCMReconfigure ccmReconfigure = new CCMReconfigure();

    // Act
    ccmReconfigure.setCcmProject("foo");
    ccmReconfigure.setRecurse(true);
    ccmReconfigure.setVerbose(true);
    String actualCcmProject = ccmReconfigure.getCcmProject();
    boolean actualIsRecurseResult = ccmReconfigure.isRecurse();

    // Assert
    assertEquals("foo", actualCcmProject);
    assertTrue(actualIsRecurseResult);
    assertTrue(ccmReconfigure.isVerbose());
  }
}
