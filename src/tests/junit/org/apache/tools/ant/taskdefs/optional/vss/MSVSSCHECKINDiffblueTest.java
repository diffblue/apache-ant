package org.apache.tools.ant.taskdefs.optional.vss;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.types.Commandline;
import org.junit.Test;

public class MSVSSCHECKINDiffblueTest {
  /**
   * Test {@link MSVSSCHECKIN#buildCmdLine()}.
   * <ul>
   *   <li>Given {@link MSVSSCHECKIN} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MSVSSCHECKIN#buildCmdLine()}
   */
  @Test
  public void testBuildCmdLine_givenMsvsscheckin_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new MSVSSCHECKIN()).buildCmdLine());
  }

  /**
   * Test {@link MSVSSCHECKIN#buildCmdLine()}.
   * <ul>
   *   <li>Then return fifth element is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link MSVSSCHECKIN#buildCmdLine()}
   */
  @Test
  public void testBuildCmdLine_thenReturnFifthElementIsEmptyString() {
    // Arrange
    MSVSSCHECKIN msvsscheckin = new MSVSSCHECKIN();
    msvsscheckin.setVsspath("foo");

    // Act
    Commandline actualBuildCmdLineResult = msvsscheckin.buildCmdLine();

    // Assert
    String[] arguments = actualBuildCmdLineResult.getArguments();
    assertEquals("", arguments[4]);
    assertEquals("", arguments[5]);
    assertEquals("", arguments[6]);
    String[] commandline = actualBuildCmdLineResult.getCommandline();
    assertEquals("", commandline[5]);
    assertEquals("", commandline[6]);
    assertEquals("", commandline[7]);
    assertEquals("-C-", arguments[7]);
    assertEquals("-C-", commandline[8]);
    assertEquals(8, arguments.length);
    assertEquals(9, commandline.length);
    assertEquals(MSVSSConstants.SS_EXE, actualBuildCmdLineResult.getExecutable());
    assertEquals(MSVSSConstants.SS_EXE, commandline[0]);
  }

  /**
   * Test {@link MSVSSCHECKIN#buildCmdLine()}.
   * <ul>
   *   <li>Then return third element is {@link MSVSSConstants#FLAG_OVERRIDE_WORKING_DIR}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MSVSSCHECKIN#buildCmdLine()}
   */
  @Test
  public void testBuildCmdLine_thenReturnThirdElementIsFlag_override_working_dir() {
    // Arrange
    MSVSSCHECKIN msvsscheckin = new MSVSSCHECKIN();
    msvsscheckin.setProject(new Project());
    msvsscheckin.setInternalLocalPath("");
    msvsscheckin.setVsspath("foo");

    // Act
    Commandline actualBuildCmdLineResult = msvsscheckin.buildCmdLine();

    // Assert
    String[] arguments = actualBuildCmdLineResult.getArguments();
    assertEquals("", arguments[4]);
    assertEquals("", arguments[5]);
    assertEquals("", arguments[6]);
    String[] commandline = actualBuildCmdLineResult.getCommandline();
    assertEquals("", commandline[5]);
    assertEquals("", commandline[6]);
    assertEquals("", commandline[7]);
    assertEquals("-C-", arguments[7]);
    assertEquals("-C-", commandline[8]);
    assertEquals(8, arguments.length);
    assertEquals(9, commandline.length);
    assertEquals(MSVSSConstants.FLAG_OVERRIDE_WORKING_DIR, arguments[2]);
    assertEquals(MSVSSConstants.FLAG_OVERRIDE_WORKING_DIR, commandline[3]);
    assertEquals(MSVSSConstants.SS_EXE, actualBuildCmdLineResult.getExecutable());
    assertEquals(MSVSSConstants.SS_EXE, commandline[0]);
  }

  /**
   * Test {@link MSVSSCHECKIN#buildCmdLine()}.
   * <ul>
   *   <li>Then return third element is {@code -GL.}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MSVSSCHECKIN#buildCmdLine()}
   */
  @Test
  public void testBuildCmdLine_thenReturnThirdElementIsGl() {
    // Arrange
    MSVSSCHECKIN msvsscheckin = new MSVSSCHECKIN();
    msvsscheckin.setProject(new Project());
    msvsscheckin.setInternalLocalPath(".");
    msvsscheckin.setVsspath("foo");

    // Act
    Commandline actualBuildCmdLineResult = msvsscheckin.buildCmdLine();

    // Assert
    String[] arguments = actualBuildCmdLineResult.getArguments();
    assertEquals("", arguments[4]);
    assertEquals("", arguments[5]);
    assertEquals("", arguments[6]);
    String[] commandline = actualBuildCmdLineResult.getCommandline();
    assertEquals("", commandline[5]);
    assertEquals("", commandline[6]);
    assertEquals("", commandline[7]);
    assertEquals("-C-", arguments[7]);
    assertEquals("-C-", commandline[8]);
    assertEquals("-GL.", arguments[2]);
    assertEquals("-GL.", commandline[3]);
    assertEquals(8, arguments.length);
    assertEquals(9, commandline.length);
    assertEquals(MSVSSConstants.SS_EXE, actualBuildCmdLineResult.getExecutable());
    assertEquals(MSVSSConstants.SS_EXE, commandline[0]);
  }

  /**
   * Test {@link MSVSSCHECKIN#buildCmdLine()}.
   * <ul>
   *   <li>Then return third element is {@code -GL..}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MSVSSCHECKIN#buildCmdLine()}
   */
  @Test
  public void testBuildCmdLine_thenReturnThirdElementIsGl2() {
    // Arrange
    MSVSSCHECKIN msvsscheckin = new MSVSSCHECKIN();
    msvsscheckin.setProject(new Project());
    msvsscheckin.setInternalLocalPath("..");
    msvsscheckin.setVsspath("foo");

    // Act
    Commandline actualBuildCmdLineResult = msvsscheckin.buildCmdLine();

    // Assert
    String[] arguments = actualBuildCmdLineResult.getArguments();
    assertEquals("", arguments[4]);
    assertEquals("", arguments[5]);
    assertEquals("", arguments[6]);
    String[] commandline = actualBuildCmdLineResult.getCommandline();
    assertEquals("", commandline[5]);
    assertEquals("", commandline[6]);
    assertEquals("", commandline[7]);
    assertEquals("-C-", arguments[7]);
    assertEquals("-C-", commandline[8]);
    assertEquals("-GL..", arguments[2]);
    assertEquals("-GL..", commandline[3]);
    assertEquals(8, arguments.length);
    assertEquals(9, commandline.length);
    assertEquals(MSVSSConstants.SS_EXE, actualBuildCmdLineResult.getExecutable());
    assertEquals(MSVSSConstants.SS_EXE, commandline[0]);
  }

  /**
   * Test {@link MSVSSCHECKIN#setRecursive(boolean)}.
   * <p>
   * Method under test: {@link MSVSSCHECKIN#setRecursive(boolean)}
   */
  @Test
  public void testSetRecursive() {
    // Arrange
    MSVSSCHECKIN msvsscheckin = new MSVSSCHECKIN();

    // Act
    msvsscheckin.setRecursive(true);

    // Assert
    assertEquals(MSVSSConstants.FLAG_RECURSION, msvsscheckin.getRecursive());
  }

  /**
   * Test {@link MSVSSCHECKIN#setWritable(boolean)}.
   * <p>
   * Method under test: {@link MSVSSCHECKIN#setWritable(boolean)}
   */
  @Test
  public void testSetWritable() {
    // Arrange
    MSVSSCHECKIN msvsscheckin = new MSVSSCHECKIN();

    // Act
    msvsscheckin.setWritable(true);

    // Assert
    assertEquals(MSVSSConstants.FLAG_WRITABLE, msvsscheckin.getWritable());
  }

  /**
   * Test {@link MSVSSCHECKIN#setComment(String)}.
   * <p>
   * Method under test: {@link MSVSSCHECKIN#setComment(String)}
   */
  @Test
  public void testSetComment() {
    // Arrange
    MSVSSCHECKIN msvsscheckin = new MSVSSCHECKIN();

    // Act
    msvsscheckin.setComment("Comment");

    // Assert
    assertEquals("-CComment", msvsscheckin.getComment());
  }

  /**
   * Test new {@link MSVSSCHECKIN} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link MSVSSCHECKIN}
   */
  @Test
  public void testNewMsvsscheckin() throws BuildException {
    // Arrange and Act
    MSVSSCHECKIN actualMsvsscheckin = new MSVSSCHECKIN();

    // Assert
    assertEquals("", actualMsvsscheckin.getFileTimeStamp());
    assertEquals("", actualMsvsscheckin.getGetLocalCopy());
    assertEquals("", actualMsvsscheckin.getLabel());
    assertEquals("", actualMsvsscheckin.getLocalpath());
    assertEquals("", actualMsvsscheckin.getLogin());
    assertEquals("", actualMsvsscheckin.getOutput());
    assertEquals("", actualMsvsscheckin.getQuiet());
    assertEquals("", actualMsvsscheckin.getRecursive());
    assertEquals("", actualMsvsscheckin.getStyle());
    assertEquals("", actualMsvsscheckin.getUser());
    assertEquals("", actualMsvsscheckin.getVersion());
    assertEquals("", actualMsvsscheckin.getVersionDate());
    assertEquals("", actualMsvsscheckin.getVersionDateLabel());
    assertEquals("", actualMsvsscheckin.getVersionLabel());
    assertEquals("", actualMsvsscheckin.getWritable());
    assertEquals("", actualMsvsscheckin.getWritableFiles());
    assertEquals("-C-", actualMsvsscheckin.getComment());
    assertNull(actualMsvsscheckin.getDescription());
    assertNull(actualMsvsscheckin.getTaskName());
    assertNull(actualMsvsscheckin.getTaskType());
    assertNull(actualMsvsscheckin.getVsspath());
    assertNull(actualMsvsscheckin.getProject());
    assertNull(actualMsvsscheckin.getOwningTarget());
    assertEquals(MSVSSConstants.FLAG_AUTORESPONSE_DEF, actualMsvsscheckin.getAutoresponse());
    assertEquals(MSVSSConstants.SS_EXE, actualMsvsscheckin.getSSCommand());
  }
}
