package org.apache.tools.ant.taskdefs.optional.sos;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import org.junit.Test;

public class SOSCheckoutDiffblueTest {
  /**
   * Test {@link SOSCheckout#setFile(String)}.
   * <p>
   * Method under test: {@link SOSCheckout#setFile(String)}
   */
  @Test
  public void testSetFile() {
    // Arrange
    SOSCheckout sosCheckout = new SOSCheckout();

    // Act
    sosCheckout.setFile("foo.txt");

    // Assert
    assertEquals("foo.txt", sosCheckout.getFilename());
  }

  /**
   * Test {@link SOSCheckout#setRecursive(boolean)}.
   * <p>
   * Method under test: {@link SOSCheckout#setRecursive(boolean)}
   */
  @Test
  public void testSetRecursive() {
    // Arrange
    SOSCheckout sosCheckout = new SOSCheckout();

    // Act
    sosCheckout.setRecursive(true);

    // Assert
    assertEquals(SOSCmd.FLAG_RECURSION, sosCheckout.getRecursive());
  }

  /**
   * Test new {@link SOSCheckout} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link SOSCheckout}
   */
  @Test
  public void testNewSOSCheckout() {
    // Arrange and Act
    SOSCheckout actualSosCheckout = new SOSCheckout();

    // Assert
    assertEquals("", actualSosCheckout.getNoCache());
    assertEquals("", actualSosCheckout.getNoCompress());
    assertEquals("", actualSosCheckout.getPassword());
    assertEquals("", actualSosCheckout.getRecursive());
    assertEquals("", actualSosCheckout.getVerbose());
    assertNull(actualSosCheckout.getDescription());
    assertNull(actualSosCheckout.getTaskName());
    assertNull(actualSosCheckout.getTaskType());
    assertNull(actualSosCheckout.getComment());
    assertNull(actualSosCheckout.getFilename());
    assertNull(actualSosCheckout.getLabel());
    assertNull(actualSosCheckout.getProjectPath());
    assertNull(actualSosCheckout.getSosHome());
    assertNull(actualSosCheckout.getSosServerPath());
    assertNull(actualSosCheckout.getUsername());
    assertNull(actualSosCheckout.getVersion());
    assertNull(actualSosCheckout.getVssServerPath());
    assertNull(actualSosCheckout.getProject());
    assertNull(actualSosCheckout.getOwningTarget());
    assertNull(actualSosCheckout.commandLine);
    assertEquals(SOSCmd.COMMAND_SOS_EXE, actualSosCheckout.getSosCommand());
  }
}
