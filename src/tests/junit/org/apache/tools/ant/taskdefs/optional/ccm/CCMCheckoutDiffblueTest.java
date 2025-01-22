package org.apache.tools.ant.taskdefs.optional.ccm;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import org.junit.Test;

public class CCMCheckoutDiffblueTest {
  /**
   * Test new {@link CCMCheckout} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link CCMCheckout}
   */
  @Test
  public void testNewCCMCheckout() {
    // Arrange and Act
    CCMCheckout actualCcmCheckout = new CCMCheckout();

    // Assert
    assertEquals("ccm", actualCcmCheckout.getCcmCommand());
    assertNull(actualCcmCheckout.getFile());
    assertNull(actualCcmCheckout.getDescription());
    assertNull(actualCcmCheckout.getTaskName());
    assertNull(actualCcmCheckout.getTaskType());
    assertNull(actualCcmCheckout.getComment());
    assertNull(actualCcmCheckout.getTask());
    assertNull(actualCcmCheckout.getProject());
    assertNull(actualCcmCheckout.getOwningTarget());
    assertTrue(actualCcmCheckout.filesets.isEmpty());
    assertEquals(Continuus.COMMAND_CHECKOUT, actualCcmCheckout.getCcmAction());
  }
}
