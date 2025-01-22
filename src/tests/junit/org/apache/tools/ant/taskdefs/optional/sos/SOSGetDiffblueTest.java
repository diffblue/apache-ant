package org.apache.tools.ant.taskdefs.optional.sos;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import org.junit.Test;

public class SOSGetDiffblueTest {
  /**
   * Test {@link SOSGet#setFile(String)}.
   * <p>
   * Method under test: {@link SOSGet#setFile(String)}
   */
  @Test
  public void testSetFile() {
    // Arrange
    SOSGet sosGet = new SOSGet();

    // Act
    sosGet.setFile("foo.txt");

    // Assert
    assertEquals("foo.txt", sosGet.getFilename());
  }

  /**
   * Test new {@link SOSGet} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link SOSGet}
   */
  @Test
  public void testNewSOSGet() {
    // Arrange and Act
    SOSGet actualSosGet = new SOSGet();

    // Assert
    assertEquals("", actualSosGet.getNoCache());
    assertEquals("", actualSosGet.getNoCompress());
    assertEquals("", actualSosGet.getPassword());
    assertEquals("", actualSosGet.getRecursive());
    assertEquals("", actualSosGet.getVerbose());
    assertNull(actualSosGet.getDescription());
    assertNull(actualSosGet.getTaskName());
    assertNull(actualSosGet.getTaskType());
    assertNull(actualSosGet.getComment());
    assertNull(actualSosGet.getFilename());
    assertNull(actualSosGet.getLabel());
    assertNull(actualSosGet.getProjectPath());
    assertNull(actualSosGet.getSosHome());
    assertNull(actualSosGet.getSosServerPath());
    assertNull(actualSosGet.getUsername());
    assertNull(actualSosGet.getVersion());
    assertNull(actualSosGet.getVssServerPath());
    assertNull(actualSosGet.getProject());
    assertNull(actualSosGet.getOwningTarget());
    assertNull(actualSosGet.commandLine);
    assertEquals(SOSCmd.COMMAND_SOS_EXE, actualSosGet.getSosCommand());
  }
}
