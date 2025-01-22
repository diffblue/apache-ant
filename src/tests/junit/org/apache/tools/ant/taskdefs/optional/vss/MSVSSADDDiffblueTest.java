package org.apache.tools.ant.taskdefs.optional.vss;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import org.apache.tools.ant.BuildException;
import org.junit.Test;

public class MSVSSADDDiffblueTest {
  /**
   * Test {@link MSVSSADD#buildCmdLine()}.
   * <ul>
   *   <li>Given {@link MSVSSADD} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MSVSSADD#buildCmdLine()}
   */
  @Test
  public void testBuildCmdLine_givenMsvssadd_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new MSVSSADD()).buildCmdLine());
  }

  /**
   * Test {@link MSVSSADD#getLocalpath()}.
   * <p>
   * Method under test: {@link MSVSSADD#getLocalpath()}
   */
  @Test
  public void testGetLocalpath() {
    // Arrange, Act and Assert
    assertNull((new MSVSSADD()).getLocalpath());
  }

  /**
   * Test {@link MSVSSADD#setRecursive(boolean)}.
   * <p>
   * Method under test: {@link MSVSSADD#setRecursive(boolean)}
   */
  @Test
  public void testSetRecursive() {
    // Arrange
    MSVSSADD msvssadd = new MSVSSADD();

    // Act
    msvssadd.setRecursive(true);

    // Assert
    assertEquals(MSVSSConstants.FLAG_RECURSION, msvssadd.getRecursive());
  }

  /**
   * Test {@link MSVSSADD#setWritable(boolean)}.
   * <p>
   * Method under test: {@link MSVSSADD#setWritable(boolean)}
   */
  @Test
  public void testSetWritable() {
    // Arrange
    MSVSSADD msvssadd = new MSVSSADD();

    // Act
    msvssadd.setWritable(true);

    // Assert
    assertEquals(MSVSSConstants.FLAG_WRITABLE, msvssadd.getWritable());
  }

  /**
   * Test {@link MSVSSADD#setComment(String)}.
   * <p>
   * Method under test: {@link MSVSSADD#setComment(String)}
   */
  @Test
  public void testSetComment() {
    // Arrange
    MSVSSADD msvssadd = new MSVSSADD();

    // Act
    msvssadd.setComment("Comment");

    // Assert
    assertEquals("-CComment", msvssadd.getComment());
  }

  /**
   * Test new {@link MSVSSADD} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link MSVSSADD}
   */
  @Test
  public void testNewMsvssadd() throws BuildException {
    // Arrange and Act
    MSVSSADD actualMsvssadd = new MSVSSADD();

    // Assert
    assertEquals("", actualMsvssadd.getFileTimeStamp());
    assertEquals("", actualMsvssadd.getGetLocalCopy());
    assertEquals("", actualMsvssadd.getLabel());
    assertEquals("", actualMsvssadd.getLogin());
    assertEquals("", actualMsvssadd.getOutput());
    assertEquals("", actualMsvssadd.getQuiet());
    assertEquals("", actualMsvssadd.getRecursive());
    assertEquals("", actualMsvssadd.getStyle());
    assertEquals("", actualMsvssadd.getUser());
    assertEquals("", actualMsvssadd.getVersion());
    assertEquals("", actualMsvssadd.getVersionDate());
    assertEquals("", actualMsvssadd.getVersionDateLabel());
    assertEquals("", actualMsvssadd.getVersionLabel());
    assertEquals("", actualMsvssadd.getWritable());
    assertEquals("", actualMsvssadd.getWritableFiles());
    assertEquals("-C-", actualMsvssadd.getComment());
    assertNull(actualMsvssadd.getDescription());
    assertNull(actualMsvssadd.getTaskName());
    assertNull(actualMsvssadd.getTaskType());
    assertNull(actualMsvssadd.getVsspath());
    assertNull(actualMsvssadd.getLocalpath());
    assertNull(actualMsvssadd.getProject());
    assertNull(actualMsvssadd.getOwningTarget());
    assertEquals(MSVSSConstants.FLAG_AUTORESPONSE_DEF, actualMsvssadd.getAutoresponse());
    assertEquals(MSVSSConstants.SS_EXE, actualMsvssadd.getSSCommand());
  }
}
