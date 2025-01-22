package org.apache.tools.ant.taskdefs.optional.vss;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.types.Commandline;
import org.junit.Test;

public class MSVSSCREATEDiffblueTest {
  /**
   * Test {@link MSVSSCREATE#buildCmdLine()}.
   * <ul>
   *   <li>Given {@link MSVSSCREATE} (default constructor) Login is {@link MSVSSConstants#SS_EXE}.</li>
   *   <li>Then return sixth element is {@code -Yss}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MSVSSCREATE#buildCmdLine()}
   */
  @Test
  public void testBuildCmdLine_givenMsvsscreateLoginIsSs_exe_thenReturnSixthElementIsYss() {
    // Arrange
    MSVSSCREATE msvsscreate = new MSVSSCREATE();
    msvsscreate.setLogin(MSVSSConstants.SS_EXE);
    msvsscreate.setVsspath("foo");

    // Act
    Commandline actualBuildCmdLineResult = msvsscreate.buildCmdLine();

    // Assert
    String[] arguments = actualBuildCmdLineResult.getArguments();
    assertEquals("", arguments[4]);
    String[] commandline = actualBuildCmdLineResult.getCommandline();
    assertEquals("", commandline[5]);
    assertEquals("-C-", arguments[2]);
    assertEquals("-C-", commandline[3]);
    assertEquals("-Yss", arguments[5]);
    assertEquals("-Yss", commandline[6]);
    assertEquals(6, arguments.length);
    assertEquals(7, commandline.length);
    assertEquals(MSVSSConstants.SS_EXE, actualBuildCmdLineResult.getExecutable());
    assertEquals(MSVSSConstants.SS_EXE, commandline[0]);
  }

  /**
   * Test {@link MSVSSCREATE#buildCmdLine()}.
   * <ul>
   *   <li>Given {@link MSVSSCREATE} (default constructor) Ssdir is {@link MSVSSConstants#SS_EXE}.</li>
   *   <li>Then return Executable is {@code ss/ss}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MSVSSCREATE#buildCmdLine()}
   */
  @Test
  public void testBuildCmdLine_givenMsvsscreateSsdirIsSs_exe_thenReturnExecutableIsSsSs() {
    // Arrange
    MSVSSCREATE msvsscreate = new MSVSSCREATE();
    msvsscreate.setSsdir(MSVSSConstants.SS_EXE);
    msvsscreate.setVsspath("foo");

    // Act
    Commandline actualBuildCmdLineResult = msvsscreate.buildCmdLine();

    // Assert
    String[] arguments = actualBuildCmdLineResult.getArguments();
    assertEquals("", arguments[4]);
    assertEquals("", arguments[5]);
    String[] commandline = actualBuildCmdLineResult.getCommandline();
    assertEquals("", commandline[5]);
    assertEquals("", commandline[6]);
    assertEquals("-C-", arguments[2]);
    assertEquals("-C-", commandline[3]);
    assertEquals("ss/ss", actualBuildCmdLineResult.getExecutable());
    assertEquals("ss/ss", commandline[0]);
    assertEquals(6, arguments.length);
    assertEquals(7, commandline.length);
  }

  /**
   * Test {@link MSVSSCREATE#buildCmdLine()}.
   * <ul>
   *   <li>Given {@link MSVSSCREATE} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MSVSSCREATE#buildCmdLine()}
   */
  @Test
  public void testBuildCmdLine_givenMsvsscreate_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new MSVSSCREATE()).buildCmdLine());
  }

  /**
   * Test {@link MSVSSCREATE#buildCmdLine()}.
   * <ul>
   *   <li>Then return fifth element is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link MSVSSCREATE#buildCmdLine()}
   */
  @Test
  public void testBuildCmdLine_thenReturnFifthElementIsEmptyString() {
    // Arrange
    MSVSSCREATE msvsscreate = new MSVSSCREATE();
    msvsscreate.setVsspath("foo");

    // Act
    Commandline actualBuildCmdLineResult = msvsscreate.buildCmdLine();

    // Assert
    String[] arguments = actualBuildCmdLineResult.getArguments();
    assertEquals("", arguments[4]);
    assertEquals("", arguments[5]);
    String[] commandline = actualBuildCmdLineResult.getCommandline();
    assertEquals("", commandline[5]);
    assertEquals("", commandline[6]);
    assertEquals("-C-", arguments[2]);
    assertEquals("-C-", commandline[3]);
    assertEquals(6, arguments.length);
    assertEquals(7, commandline.length);
    assertEquals(MSVSSConstants.SS_EXE, actualBuildCmdLineResult.getExecutable());
    assertEquals(MSVSSConstants.SS_EXE, commandline[0]);
  }

  /**
   * Test {@link MSVSSCREATE#buildCmdLine()}.
   * <ul>
   *   <li>Then return fifth element is {@link MSVSSConstants#FLAG_QUIET}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MSVSSCREATE#buildCmdLine()}
   */
  @Test
  public void testBuildCmdLine_thenReturnFifthElementIsFlag_quiet() {
    // Arrange
    MSVSSCREATE msvsscreate = new MSVSSCREATE();
    msvsscreate.setInternalQuiet(true);
    msvsscreate.setVsspath("foo");

    // Act
    Commandline actualBuildCmdLineResult = msvsscreate.buildCmdLine();

    // Assert
    String[] arguments = actualBuildCmdLineResult.getArguments();
    assertEquals("", arguments[5]);
    String[] commandline = actualBuildCmdLineResult.getCommandline();
    assertEquals("", commandline[6]);
    assertEquals("-C-", arguments[2]);
    assertEquals("-C-", commandline[3]);
    assertEquals(6, arguments.length);
    assertEquals(7, commandline.length);
    assertEquals(MSVSSConstants.FLAG_QUIET, arguments[4]);
    assertEquals(MSVSSConstants.FLAG_QUIET, commandline[5]);
    assertEquals(MSVSSConstants.SS_EXE, actualBuildCmdLineResult.getExecutable());
    assertEquals(MSVSSConstants.SS_EXE, commandline[0]);
  }

  /**
   * Test {@link MSVSSCREATE#buildCmdLine()}.
   * <ul>
   *   <li>Then return third element is {@code -Css}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MSVSSCREATE#buildCmdLine()}
   */
  @Test
  public void testBuildCmdLine_thenReturnThirdElementIsCss() {
    // Arrange
    MSVSSCREATE msvsscreate = new MSVSSCREATE();
    msvsscreate.setInternalComment(MSVSSConstants.SS_EXE);
    msvsscreate.setVsspath("foo");

    // Act
    Commandline actualBuildCmdLineResult = msvsscreate.buildCmdLine();

    // Assert
    String[] arguments = actualBuildCmdLineResult.getArguments();
    assertEquals("", arguments[4]);
    assertEquals("", arguments[5]);
    String[] commandline = actualBuildCmdLineResult.getCommandline();
    assertEquals("", commandline[5]);
    assertEquals("", commandline[6]);
    assertEquals("-Css", arguments[2]);
    assertEquals("-Css", commandline[3]);
    assertEquals(6, arguments.length);
    assertEquals(7, commandline.length);
    assertEquals(MSVSSConstants.SS_EXE, actualBuildCmdLineResult.getExecutable());
    assertEquals(MSVSSConstants.SS_EXE, commandline[0]);
  }

  /**
   * Test {@link MSVSSCREATE#setComment(String)}.
   * <p>
   * Method under test: {@link MSVSSCREATE#setComment(String)}
   */
  @Test
  public void testSetComment() {
    // Arrange
    MSVSSCREATE msvsscreate = new MSVSSCREATE();

    // Act
    msvsscreate.setComment("Comment");

    // Assert
    assertEquals("-CComment", msvsscreate.getComment());
  }

  /**
   * Test {@link MSVSSCREATE#setQuiet(boolean)}.
   * <p>
   * Method under test: {@link MSVSSCREATE#setQuiet(boolean)}
   */
  @Test
  public void testSetQuiet() {
    // Arrange
    MSVSSCREATE msvsscreate = new MSVSSCREATE();

    // Act
    msvsscreate.setQuiet(true);

    // Assert
    assertEquals(MSVSSConstants.FLAG_QUIET, msvsscreate.getQuiet());
  }

  /**
   * Test new {@link MSVSSCREATE} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link MSVSSCREATE}
   */
  @Test
  public void testNewMsvsscreate() throws BuildException {
    // Arrange and Act
    MSVSSCREATE actualMsvsscreate = new MSVSSCREATE();

    // Assert
    assertEquals("", actualMsvsscreate.getFileTimeStamp());
    assertEquals("", actualMsvsscreate.getGetLocalCopy());
    assertEquals("", actualMsvsscreate.getLabel());
    assertEquals("", actualMsvsscreate.getLocalpath());
    assertEquals("", actualMsvsscreate.getLogin());
    assertEquals("", actualMsvsscreate.getOutput());
    assertEquals("", actualMsvsscreate.getQuiet());
    assertEquals("", actualMsvsscreate.getRecursive());
    assertEquals("", actualMsvsscreate.getStyle());
    assertEquals("", actualMsvsscreate.getUser());
    assertEquals("", actualMsvsscreate.getVersion());
    assertEquals("", actualMsvsscreate.getVersionDate());
    assertEquals("", actualMsvsscreate.getVersionDateLabel());
    assertEquals("", actualMsvsscreate.getVersionLabel());
    assertEquals("", actualMsvsscreate.getWritable());
    assertEquals("", actualMsvsscreate.getWritableFiles());
    assertEquals("-C-", actualMsvsscreate.getComment());
    assertNull(actualMsvsscreate.getDescription());
    assertNull(actualMsvsscreate.getTaskName());
    assertNull(actualMsvsscreate.getTaskType());
    assertNull(actualMsvsscreate.getVsspath());
    assertNull(actualMsvsscreate.getProject());
    assertNull(actualMsvsscreate.getOwningTarget());
    assertEquals(MSVSSConstants.FLAG_AUTORESPONSE_DEF, actualMsvsscreate.getAutoresponse());
    assertEquals(MSVSSConstants.SS_EXE, actualMsvsscreate.getSSCommand());
  }
}
