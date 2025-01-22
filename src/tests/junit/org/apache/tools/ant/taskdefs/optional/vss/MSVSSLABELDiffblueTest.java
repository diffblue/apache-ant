package org.apache.tools.ant.taskdefs.optional.vss;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.types.Commandline;
import org.junit.Test;

public class MSVSSLABELDiffblueTest {
  /**
   * Test {@link MSVSSLABEL#buildCmdLine()}.
   * <ul>
   *   <li>Given {@link MSVSSLABEL} (default constructor) InternalLabel is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link MSVSSLABEL#buildCmdLine()}
   */
  @Test
  public void testBuildCmdLine_givenMsvsslabelInternalLabelIsEmptyString() {
    // Arrange
    MSVSSLABEL msvsslabel = new MSVSSLABEL();
    msvsslabel.setInternalLabel("");
    msvsslabel.setVsspath("foo");

    // Act and Assert
    assertThrows(BuildException.class, () -> msvsslabel.buildCmdLine());
  }

  /**
   * Test {@link MSVSSLABEL#buildCmdLine()}.
   * <ul>
   *   <li>Given {@link MSVSSLABEL} (default constructor) Login is {@link MSVSSConstants#FLAG_LABEL}.</li>
   *   <li>Then return seventh element is {@code -Y-L}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MSVSSLABEL#buildCmdLine()}
   */
  @Test
  public void testBuildCmdLine_givenMsvsslabelLoginIsFlag_label_thenReturnSeventhElementIsYL() {
    // Arrange
    MSVSSLABEL msvsslabel = new MSVSSLABEL();
    msvsslabel.setLogin(MSVSSConstants.FLAG_LABEL);
    msvsslabel.setInternalLabel("label attribute must be set!");
    msvsslabel.setVsspath("foo");

    // Act
    Commandline actualBuildCmdLineResult = msvsslabel.buildCmdLine();

    // Assert
    String[] arguments = actualBuildCmdLineResult.getArguments();
    assertEquals("", arguments[5]);
    String[] commandline = actualBuildCmdLineResult.getCommandline();
    assertEquals("", commandline[6]);
    assertEquals("-C-", arguments[2]);
    assertEquals("-C-", commandline[3]);
    assertEquals("-Y-L", arguments[6]);
    assertEquals("-Y-L", commandline[7]);
    assertEquals(7, arguments.length);
    assertEquals(8, commandline.length);
    assertEquals(MSVSSConstants.SS_EXE, actualBuildCmdLineResult.getExecutable());
    assertEquals(MSVSSConstants.SS_EXE, commandline[0]);
  }

  /**
   * Test {@link MSVSSLABEL#buildCmdLine()}.
   * <ul>
   *   <li>Given {@link MSVSSLABEL} (default constructor) Ssdir is {@link MSVSSConstants#FLAG_LABEL}.</li>
   *   <li>Then return Executable is {@code -L/ss}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MSVSSLABEL#buildCmdLine()}
   */
  @Test
  public void testBuildCmdLine_givenMsvsslabelSsdirIsFlag_label_thenReturnExecutableIsLSs() {
    // Arrange
    MSVSSLABEL msvsslabel = new MSVSSLABEL();
    msvsslabel.setSsdir(MSVSSConstants.FLAG_LABEL);
    msvsslabel.setInternalLabel("label attribute must be set!");
    msvsslabel.setVsspath("foo");

    // Act
    Commandline actualBuildCmdLineResult = msvsslabel.buildCmdLine();

    // Assert
    String[] arguments = actualBuildCmdLineResult.getArguments();
    assertEquals("", arguments[5]);
    assertEquals("", arguments[6]);
    String[] commandline = actualBuildCmdLineResult.getCommandline();
    assertEquals("", commandline[6]);
    assertEquals("", commandline[7]);
    assertEquals("-C-", arguments[2]);
    assertEquals("-C-", commandline[3]);
    assertEquals("-L/ss", actualBuildCmdLineResult.getExecutable());
    assertEquals("-L/ss", commandline[0]);
    assertEquals(7, arguments.length);
    assertEquals(8, commandline.length);
  }

  /**
   * Test {@link MSVSSLABEL#buildCmdLine()}.
   * <ul>
   *   <li>Given {@link MSVSSLABEL} (default constructor) Vsspath is {@code foo}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MSVSSLABEL#buildCmdLine()}
   */
  @Test
  public void testBuildCmdLine_givenMsvsslabelVsspathIsFoo_thenThrowBuildException() {
    // Arrange
    MSVSSLABEL msvsslabel = new MSVSSLABEL();
    msvsslabel.setVsspath("foo");

    // Act and Assert
    assertThrows(BuildException.class, () -> msvsslabel.buildCmdLine());
  }

  /**
   * Test {@link MSVSSLABEL#buildCmdLine()}.
   * <ul>
   *   <li>Given {@link MSVSSLABEL} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MSVSSLABEL#buildCmdLine()}
   */
  @Test
  public void testBuildCmdLine_givenMsvsslabel_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new MSVSSLABEL()).buildCmdLine());
  }

  /**
   * Test {@link MSVSSLABEL#buildCmdLine()}.
   * <ul>
   *   <li>Then return sixth element is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link MSVSSLABEL#buildCmdLine()}
   */
  @Test
  public void testBuildCmdLine_thenReturnSixthElementIsEmptyString() {
    // Arrange
    MSVSSLABEL msvsslabel = new MSVSSLABEL();
    msvsslabel.setInternalLabel("label attribute must be set!");
    msvsslabel.setVsspath("foo");

    // Act
    Commandline actualBuildCmdLineResult = msvsslabel.buildCmdLine();

    // Assert
    String[] arguments = actualBuildCmdLineResult.getArguments();
    assertEquals("", arguments[5]);
    assertEquals("", arguments[6]);
    String[] commandline = actualBuildCmdLineResult.getCommandline();
    assertEquals("", commandline[6]);
    assertEquals("", commandline[7]);
    assertEquals("-C-", arguments[2]);
    assertEquals("-C-", commandline[3]);
    assertEquals(7, arguments.length);
    assertEquals(8, commandline.length);
    assertEquals(MSVSSConstants.SS_EXE, actualBuildCmdLineResult.getExecutable());
    assertEquals(MSVSSConstants.SS_EXE, commandline[0]);
  }

  /**
   * Test {@link MSVSSLABEL#buildCmdLine()}.
   * <ul>
   *   <li>Then return sixth element is {@code -V1.0.2}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MSVSSLABEL#buildCmdLine()}
   */
  @Test
  public void testBuildCmdLine_thenReturnSixthElementIsV102() {
    // Arrange
    MSVSSLABEL msvsslabel = new MSVSSLABEL();
    msvsslabel.setInternalVersion("1.0.2");
    msvsslabel.setInternalLabel("label attribute must be set!");
    msvsslabel.setVsspath("foo");

    // Act
    Commandline actualBuildCmdLineResult = msvsslabel.buildCmdLine();

    // Assert
    String[] arguments = actualBuildCmdLineResult.getArguments();
    assertEquals("", arguments[6]);
    String[] commandline = actualBuildCmdLineResult.getCommandline();
    assertEquals("", commandline[7]);
    assertEquals("-C-", arguments[2]);
    assertEquals("-C-", commandline[3]);
    assertEquals("-V1.0.2", arguments[5]);
    assertEquals("-V1.0.2", commandline[6]);
    assertEquals(7, arguments.length);
    assertEquals(8, commandline.length);
    assertEquals(MSVSSConstants.SS_EXE, actualBuildCmdLineResult.getExecutable());
    assertEquals(MSVSSConstants.SS_EXE, commandline[0]);
  }

  /**
   * Test {@link MSVSSLABEL#buildCmdLine()}.
   * <ul>
   *   <li>Then return third element is {@code -C-L}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MSVSSLABEL#buildCmdLine()}
   */
  @Test
  public void testBuildCmdLine_thenReturnThirdElementIsCL() {
    // Arrange
    MSVSSLABEL msvsslabel = new MSVSSLABEL();
    msvsslabel.setInternalComment(MSVSSConstants.FLAG_LABEL);
    msvsslabel.setInternalLabel("label attribute must be set!");
    msvsslabel.setVsspath("foo");

    // Act
    Commandline actualBuildCmdLineResult = msvsslabel.buildCmdLine();

    // Assert
    String[] arguments = actualBuildCmdLineResult.getArguments();
    assertEquals("", arguments[5]);
    assertEquals("", arguments[6]);
    String[] commandline = actualBuildCmdLineResult.getCommandline();
    assertEquals("", commandline[6]);
    assertEquals("", commandline[7]);
    assertEquals("-C-L", arguments[2]);
    assertEquals("-C-L", commandline[3]);
    assertEquals(7, arguments.length);
    assertEquals(8, commandline.length);
    assertEquals(MSVSSConstants.SS_EXE, actualBuildCmdLineResult.getExecutable());
    assertEquals(MSVSSConstants.SS_EXE, commandline[0]);
  }

  /**
   * Test {@link MSVSSLABEL#setLabel(String)}.
   * <p>
   * Method under test: {@link MSVSSLABEL#setLabel(String)}
   */
  @Test
  public void testSetLabel() {
    // Arrange
    MSVSSLABEL msvsslabel = new MSVSSLABEL();

    // Act
    msvsslabel.setLabel(MSVSSConstants.COMMAND_LABEL);

    // Assert
    assertEquals("-LLabel", msvsslabel.getLabel());
    assertEquals("-VLLabel", msvsslabel.getVersionDateLabel());
  }

  /**
   * Test {@link MSVSSLABEL#setVersion(String)}.
   * <p>
   * Method under test: {@link MSVSSLABEL#setVersion(String)}
   */
  @Test
  public void testSetVersion() {
    // Arrange
    MSVSSLABEL msvsslabel = new MSVSSLABEL();

    // Act
    msvsslabel.setVersion("1.0.2");

    // Assert
    assertEquals("-V1.0.2", msvsslabel.getVersion());
    assertEquals("-V1.0.2", msvsslabel.getVersionDateLabel());
  }

  /**
   * Test {@link MSVSSLABEL#setComment(String)}.
   * <p>
   * Method under test: {@link MSVSSLABEL#setComment(String)}
   */
  @Test
  public void testSetComment() {
    // Arrange
    MSVSSLABEL msvsslabel = new MSVSSLABEL();

    // Act
    msvsslabel.setComment("Comment");

    // Assert
    assertEquals("-CComment", msvsslabel.getComment());
  }

  /**
   * Test new {@link MSVSSLABEL} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link MSVSSLABEL}
   */
  @Test
  public void testNewMsvsslabel() throws BuildException {
    // Arrange and Act
    MSVSSLABEL actualMsvsslabel = new MSVSSLABEL();

    // Assert
    assertEquals("", actualMsvsslabel.getFileTimeStamp());
    assertEquals("", actualMsvsslabel.getGetLocalCopy());
    assertEquals("", actualMsvsslabel.getLabel());
    assertEquals("", actualMsvsslabel.getLocalpath());
    assertEquals("", actualMsvsslabel.getLogin());
    assertEquals("", actualMsvsslabel.getOutput());
    assertEquals("", actualMsvsslabel.getQuiet());
    assertEquals("", actualMsvsslabel.getRecursive());
    assertEquals("", actualMsvsslabel.getStyle());
    assertEquals("", actualMsvsslabel.getUser());
    assertEquals("", actualMsvsslabel.getVersion());
    assertEquals("", actualMsvsslabel.getVersionDate());
    assertEquals("", actualMsvsslabel.getVersionDateLabel());
    assertEquals("", actualMsvsslabel.getVersionLabel());
    assertEquals("", actualMsvsslabel.getWritable());
    assertEquals("", actualMsvsslabel.getWritableFiles());
    assertEquals("-C-", actualMsvsslabel.getComment());
    assertNull(actualMsvsslabel.getDescription());
    assertNull(actualMsvsslabel.getTaskName());
    assertNull(actualMsvsslabel.getTaskType());
    assertNull(actualMsvsslabel.getVsspath());
    assertNull(actualMsvsslabel.getProject());
    assertNull(actualMsvsslabel.getOwningTarget());
    assertEquals(MSVSSConstants.FLAG_AUTORESPONSE_DEF, actualMsvsslabel.getAutoresponse());
    assertEquals(MSVSSConstants.SS_EXE, actualMsvsslabel.getSSCommand());
  }
}
