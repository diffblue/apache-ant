package org.apache.tools.ant.taskdefs.optional.sos;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.types.Commandline;
import org.junit.Test;

public class SOSLabelDiffblueTest {
  /**
   * Test {@link SOSLabel#setVersion(String)}.
   * <p>
   * Method under test: {@link SOSLabel#setVersion(String)}
   */
  @Test
  public void testSetVersion() {
    // Arrange
    SOSLabel sosLabel = new SOSLabel();

    // Act
    sosLabel.setVersion("1.0.2");

    // Assert
    assertEquals("1.0.2", sosLabel.getVersion());
  }

  /**
   * Test {@link SOSLabel#setLabel(String)}.
   * <p>
   * Method under test: {@link SOSLabel#setLabel(String)}
   */
  @Test
  public void testSetLabel() {
    // Arrange
    SOSLabel sosLabel = new SOSLabel();

    // Act
    sosLabel.setLabel("Label");

    // Assert
    assertEquals("Label", sosLabel.getLabel());
  }

  /**
   * Test {@link SOSLabel#setComment(String)}.
   * <p>
   * Method under test: {@link SOSLabel#setComment(String)}
   */
  @Test
  public void testSetComment() {
    // Arrange
    SOSLabel sosLabel = new SOSLabel();

    // Act
    sosLabel.setComment("Comment");

    // Assert
    assertEquals("Comment", sosLabel.getComment());
  }

  /**
   * Test {@link SOSLabel#buildCmdLine()}.
   * <ul>
   *   <li>Given {@link SOSLabel} (default constructor) ProjectPath is {@link SOSCmd#FLAG_COMMAND}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SOSLabel#buildCmdLine()}
   */
  @Test
  public void testBuildCmdLine_givenSOSLabelProjectPathIsFlag_command_thenThrowBuildException() {
    // Arrange
    SOSLabel sosLabel = new SOSLabel();
    sosLabel.setProjectPath(SOSCmd.FLAG_COMMAND);
    sosLabel.setVssServerPath(SOSCmd.FLAG_COMMAND);
    sosLabel.setUsername("janedoe");
    sosLabel.setSosServerPath("windows");

    // Act and Assert
    assertThrows(BuildException.class, () -> sosLabel.buildCmdLine());
  }

  /**
   * Test {@link SOSLabel#buildCmdLine()}.
   * <ul>
   *   <li>Then return array length is seventeen.</li>
   * </ul>
   * <p>
   * Method under test: {@link SOSLabel#buildCmdLine()}
   */
  @Test
  public void testBuildCmdLine_thenReturnArrayLengthIsSeventeen() {
    // Arrange
    SOSLabel sosLabel = new SOSLabel();
    sosLabel.setInternalComment(SOSCmd.FLAG_COMMAND);
    sosLabel.setInternalLabel(SOSCmd.FLAG_COMMAND);
    sosLabel.setProjectPath(SOSCmd.FLAG_COMMAND);
    sosLabel.setVssServerPath(SOSCmd.FLAG_COMMAND);
    sosLabel.setUsername("janedoe");
    sosLabel.setSosServerPath("windows");

    // Act
    Commandline actualBuildCmdLineResult = sosLabel.buildCmdLine();

    // Assert
    String[] arguments = actualBuildCmdLineResult.getArguments();
    assertEquals(17, arguments.length);
    Commandline commandline = sosLabel.commandLine;
    assertEquals(17, commandline.getArguments().length);
    assertEquals(18, actualBuildCmdLineResult.size());
    assertEquals(18, commandline.size());
    String[] commandline2 = actualBuildCmdLineResult.getCommandline();
    assertEquals(18, commandline2.length);
    assertEquals(18, commandline.getCommandline().length);
    assertEquals(SOSCmd.FLAG_COMMAND, arguments[Short.SIZE]);
    assertEquals(SOSCmd.FLAG_COMMAND, commandline2[17]);
    assertEquals(SOSCmd.FLAG_COMMENT, arguments[15]);
    assertEquals(SOSCmd.FLAG_COMMENT, commandline2[Short.SIZE]);
  }

  /**
   * Test {@link SOSLabel#buildCmdLine()}.
   * <ul>
   *   <li>Then return fifteenth element is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link SOSLabel#buildCmdLine()}
   */
  @Test
  public void testBuildCmdLine_thenReturnFifteenthElementIsEmptyString() {
    // Arrange
    SOSLabel sosLabel = new SOSLabel();
    sosLabel.setInternalLabel(SOSCmd.FLAG_COMMAND);
    sosLabel.setProjectPath(SOSCmd.FLAG_COMMAND);
    sosLabel.setVssServerPath(SOSCmd.FLAG_COMMAND);
    sosLabel.setUsername("janedoe");
    sosLabel.setSosServerPath("windows");

    // Act
    Commandline actualBuildCmdLineResult = sosLabel.buildCmdLine();

    // Assert
    String[] arguments = actualBuildCmdLineResult.getArguments();
    assertEquals("", arguments[14]);
    Commandline commandline = sosLabel.commandLine;
    String[] arguments2 = commandline.getArguments();
    assertEquals("", arguments2[14]);
    String[] commandline2 = actualBuildCmdLineResult.getCommandline();
    assertEquals("", commandline2[15]);
    String[] commandline3 = commandline.getCommandline();
    assertEquals("", commandline3[15]);
    assertEquals(15, arguments.length);
    assertEquals(15, arguments2.length);
    assertEquals(Short.SIZE, actualBuildCmdLineResult.size());
    assertEquals(Short.SIZE, commandline.size());
    assertEquals(Short.SIZE, commandline2.length);
    assertEquals(Short.SIZE, commandline3.length);
  }

  /**
   * Test {@link SOSLabel#buildCmdLine()}.
   * <ul>
   *   <li>Then return fifteenth element is {@link SOSCmd#FLAG_VERBOSE}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SOSLabel#buildCmdLine()}
   */
  @Test
  public void testBuildCmdLine_thenReturnFifteenthElementIsFlag_verbose() {
    // Arrange
    SOSLabel sosLabel = new SOSLabel();
    sosLabel.setVerbose(true);
    sosLabel.setInternalLabel(SOSCmd.FLAG_COMMAND);
    sosLabel.setProjectPath(SOSCmd.FLAG_COMMAND);
    sosLabel.setVssServerPath(SOSCmd.FLAG_COMMAND);
    sosLabel.setUsername("janedoe");
    sosLabel.setSosServerPath("windows");

    // Act
    Commandline actualBuildCmdLineResult = sosLabel.buildCmdLine();

    // Assert
    String[] arguments = actualBuildCmdLineResult.getArguments();
    assertEquals(15, arguments.length);
    Commandline commandline = sosLabel.commandLine;
    String[] arguments2 = commandline.getArguments();
    assertEquals(15, arguments2.length);
    String[] commandline2 = actualBuildCmdLineResult.getCommandline();
    assertEquals(Short.SIZE, commandline2.length);
    String[] commandline3 = commandline.getCommandline();
    assertEquals(Short.SIZE, commandline3.length);
    assertEquals(SOSCmd.FLAG_VERBOSE, arguments[14]);
    assertEquals(SOSCmd.FLAG_VERBOSE, arguments2[14]);
    assertEquals(SOSCmd.FLAG_VERBOSE, commandline2[15]);
    assertEquals(SOSCmd.FLAG_VERBOSE, commandline3[15]);
  }

  /**
   * Test new {@link SOSLabel} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link SOSLabel}
   */
  @Test
  public void testNewSOSLabel() {
    // Arrange and Act
    SOSLabel actualSosLabel = new SOSLabel();

    // Assert
    assertEquals("", actualSosLabel.getNoCache());
    assertEquals("", actualSosLabel.getNoCompress());
    assertEquals("", actualSosLabel.getPassword());
    assertEquals("", actualSosLabel.getRecursive());
    assertEquals("", actualSosLabel.getVerbose());
    assertNull(actualSosLabel.getDescription());
    assertNull(actualSosLabel.getTaskName());
    assertNull(actualSosLabel.getTaskType());
    assertNull(actualSosLabel.getComment());
    assertNull(actualSosLabel.getFilename());
    assertNull(actualSosLabel.getLabel());
    assertNull(actualSosLabel.getProjectPath());
    assertNull(actualSosLabel.getSosHome());
    assertNull(actualSosLabel.getSosServerPath());
    assertNull(actualSosLabel.getUsername());
    assertNull(actualSosLabel.getVersion());
    assertNull(actualSosLabel.getVssServerPath());
    assertNull(actualSosLabel.getProject());
    assertNull(actualSosLabel.getOwningTarget());
    assertNull(actualSosLabel.commandLine);
    assertEquals(SOSCmd.COMMAND_SOS_EXE, actualSosLabel.getSosCommand());
  }
}
