package org.apache.tools.ant.taskdefs.optional.clearcase;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import org.junit.Test;

public class CCUnCheckoutDiffblueTest {
  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link CCUnCheckout#setKeepCopy(boolean)}
   *   <li>{@link CCUnCheckout#getKeepCopy()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    CCUnCheckout ccUnCheckout = new CCUnCheckout();

    // Act
    ccUnCheckout.setKeepCopy(true);

    // Assert
    assertTrue(ccUnCheckout.getKeepCopy());
  }

  /**
   * Test new {@link CCUnCheckout} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link CCUnCheckout}
   */
  @Test
  public void testNewCCUnCheckout() {
    // Arrange and Act
    CCUnCheckout actualCcUnCheckout = new CCUnCheckout();

    // Assert
    assertEquals("cleartool", actualCcUnCheckout.getClearToolCommand());
    assertNull(actualCcUnCheckout.getDescription());
    assertNull(actualCcUnCheckout.getTaskName());
    assertNull(actualCcUnCheckout.getTaskType());
    assertNull(actualCcUnCheckout.getObjSelect());
    assertNull(actualCcUnCheckout.getViewPath());
    assertNull(actualCcUnCheckout.getProject());
    assertNull(actualCcUnCheckout.getOwningTarget());
    assertFalse(actualCcUnCheckout.getKeepCopy());
    assertTrue(actualCcUnCheckout.getFailOnErr());
  }
}
