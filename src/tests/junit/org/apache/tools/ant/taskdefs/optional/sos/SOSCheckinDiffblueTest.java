package org.apache.tools.ant.taskdefs.optional.sos;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import org.junit.Test;

public class SOSCheckinDiffblueTest {
  /**
   * Test {@link SOSCheckin#setFile(String)}.
   * <p>
   * Method under test: {@link SOSCheckin#setFile(String)}
   */
  @Test
  public void testSetFile() {
    // Arrange
    SOSCheckin sosCheckin = new SOSCheckin();

    // Act
    sosCheckin.setFile("foo.txt");

    // Assert
    assertEquals("foo.txt", sosCheckin.getFilename());
  }

  /**
   * Test {@link SOSCheckin#setRecursive(boolean)}.
   * <p>
   * Method under test: {@link SOSCheckin#setRecursive(boolean)}
   */
  @Test
  public void testSetRecursive() {
    // Arrange
    SOSCheckin sosCheckin = new SOSCheckin();

    // Act
    sosCheckin.setRecursive(true);

    // Assert
    assertEquals(SOSCmd.FLAG_RECURSION, sosCheckin.getRecursive());
  }

  /**
   * Test {@link SOSCheckin#setComment(String)}.
   * <p>
   * Method under test: {@link SOSCheckin#setComment(String)}
   */
  @Test
  public void testSetComment() {
    // Arrange
    SOSCheckin sosCheckin = new SOSCheckin();

    // Act
    sosCheckin.setComment("Comment");

    // Assert
    assertEquals("Comment", sosCheckin.getComment());
  }

  /**
   * Test new {@link SOSCheckin} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link SOSCheckin}
   */
  @Test
  public void testNewSOSCheckin() {
    // Arrange and Act
    SOSCheckin actualSosCheckin = new SOSCheckin();

    // Assert
    assertEquals("", actualSosCheckin.getNoCache());
    assertEquals("", actualSosCheckin.getNoCompress());
    assertEquals("", actualSosCheckin.getPassword());
    assertEquals("", actualSosCheckin.getRecursive());
    assertEquals("", actualSosCheckin.getVerbose());
    assertNull(actualSosCheckin.getDescription());
    assertNull(actualSosCheckin.getTaskName());
    assertNull(actualSosCheckin.getTaskType());
    assertNull(actualSosCheckin.getComment());
    assertNull(actualSosCheckin.getFilename());
    assertNull(actualSosCheckin.getLabel());
    assertNull(actualSosCheckin.getProjectPath());
    assertNull(actualSosCheckin.getSosHome());
    assertNull(actualSosCheckin.getSosServerPath());
    assertNull(actualSosCheckin.getUsername());
    assertNull(actualSosCheckin.getVersion());
    assertNull(actualSosCheckin.getVssServerPath());
    assertNull(actualSosCheckin.getProject());
    assertNull(actualSosCheckin.getOwningTarget());
    assertNull(actualSosCheckin.commandLine);
    assertEquals(SOSCmd.COMMAND_SOS_EXE, actualSosCheckin.getSosCommand());
  }
}
