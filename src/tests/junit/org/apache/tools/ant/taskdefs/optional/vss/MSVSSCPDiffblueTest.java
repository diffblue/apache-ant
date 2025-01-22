package org.apache.tools.ant.taskdefs.optional.vss;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import org.apache.tools.ant.BuildException;
import org.junit.Test;

public class MSVSSCPDiffblueTest {
  /**
   * Test {@link MSVSSCP#buildCmdLine()}.
   * <ul>
   *   <li>Given {@link MSVSSCP} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MSVSSCP#buildCmdLine()}
   */
  @Test
  public void testBuildCmdLine_givenMsvsscp_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new MSVSSCP()).buildCmdLine());
  }

  /**
   * Test new {@link MSVSSCP} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link MSVSSCP}
   */
  @Test
  public void testNewMsvsscp() throws BuildException {
    // Arrange and Act
    MSVSSCP actualMsvsscp = new MSVSSCP();

    // Assert
    assertEquals("", actualMsvsscp.getFileTimeStamp());
    assertEquals("", actualMsvsscp.getGetLocalCopy());
    assertEquals("", actualMsvsscp.getLabel());
    assertEquals("", actualMsvsscp.getLocalpath());
    assertEquals("", actualMsvsscp.getLogin());
    assertEquals("", actualMsvsscp.getOutput());
    assertEquals("", actualMsvsscp.getQuiet());
    assertEquals("", actualMsvsscp.getRecursive());
    assertEquals("", actualMsvsscp.getStyle());
    assertEquals("", actualMsvsscp.getUser());
    assertEquals("", actualMsvsscp.getVersion());
    assertEquals("", actualMsvsscp.getVersionDate());
    assertEquals("", actualMsvsscp.getVersionDateLabel());
    assertEquals("", actualMsvsscp.getVersionLabel());
    assertEquals("", actualMsvsscp.getWritable());
    assertEquals("", actualMsvsscp.getWritableFiles());
    assertEquals("-C-", actualMsvsscp.getComment());
    assertNull(actualMsvsscp.getDescription());
    assertNull(actualMsvsscp.getTaskName());
    assertNull(actualMsvsscp.getTaskType());
    assertNull(actualMsvsscp.getVsspath());
    assertNull(actualMsvsscp.getProject());
    assertNull(actualMsvsscp.getOwningTarget());
    assertEquals(MSVSSConstants.FLAG_AUTORESPONSE_DEF, actualMsvsscp.getAutoresponse());
    assertEquals(MSVSSConstants.SS_EXE, actualMsvsscp.getSSCommand());
  }
}
