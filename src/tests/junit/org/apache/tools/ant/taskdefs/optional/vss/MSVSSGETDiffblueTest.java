package org.apache.tools.ant.taskdefs.optional.vss;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.util.Iterator;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.types.Commandline;
import org.apache.tools.ant.types.Commandline.Argument;
import org.junit.Test;

public class MSVSSGETDiffblueTest {
  /**
   * Test {@link MSVSSGET#buildCmdLine()}.
   * <ul>
   *   <li>Given {@link MSVSSGET} (default constructor) InternalQuiet is {@code true}.</li>
   *   <li>Then return array length is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link MSVSSGET#buildCmdLine()}
   */
  @Test
  public void testBuildCmdLine_givenMsvssgetInternalQuietIsTrue_thenReturnArrayLengthIsOne() {
    // Arrange
    MSVSSGET msvssget = new MSVSSGET();
    msvssget.setInternalQuiet(true);
    msvssget.setVsspath("foo");

    // Act
    Commandline actualBuildCmdLineResult = msvssget.buildCmdLine();

    // Assert
    String[] arguments = actualBuildCmdLineResult.getArguments();
    assertEquals("", arguments[2]);
    assertEquals("", arguments[6]);
    String[] commandline = actualBuildCmdLineResult.getCommandline();
    assertEquals("", commandline[3]);
    assertEquals("", commandline[7]);
    Iterator<Argument> iteratorResult = actualBuildCmdLineResult.iterator();
    assertEquals(1, iteratorResult.next().getParts().length);
    assertEquals(11, arguments.length);
    assertEquals(12, commandline.length);
    assertTrue(iteratorResult.hasNext());
    assertEquals(MSVSSConstants.FLAG_QUIET, arguments[4]);
    assertEquals(MSVSSConstants.FLAG_QUIET, commandline[5]);
  }

  /**
   * Test {@link MSVSSGET#buildCmdLine()}.
   * <ul>
   *   <li>Given {@link MSVSSGET} (default constructor) Ssdir is {@link MSVSSConstants#SS_EXE}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MSVSSGET#buildCmdLine()}
   */
  @Test
  public void testBuildCmdLine_givenMsvssgetSsdirIsSs_exe_thenThrowBuildException() {
    // Arrange
    MSVSSGET msvssget = new MSVSSGET();
    msvssget.setSsdir(MSVSSConstants.SS_EXE);

    // Act and Assert
    assertThrows(BuildException.class, () -> msvssget.buildCmdLine());
  }

  /**
   * Test {@link MSVSSGET#buildCmdLine()}.
   * <ul>
   *   <li>Given {@link MSVSSGET} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MSVSSGET#buildCmdLine()}
   */
  @Test
  public void testBuildCmdLine_givenMsvssget_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new MSVSSGET()).buildCmdLine());
  }

  /**
   * Test {@link MSVSSGET#setRecursive(boolean)}.
   * <p>
   * Method under test: {@link MSVSSGET#setRecursive(boolean)}
   */
  @Test
  public void testSetRecursive() {
    // Arrange
    MSVSSGET msvssget = new MSVSSGET();

    // Act
    msvssget.setRecursive(true);

    // Assert
    assertEquals(MSVSSConstants.FLAG_RECURSION, msvssget.getRecursive());
  }

  /**
   * Test {@link MSVSSGET#setQuiet(boolean)}.
   * <p>
   * Method under test: {@link MSVSSGET#setQuiet(boolean)}
   */
  @Test
  public void testSetQuiet() {
    // Arrange
    MSVSSGET msvssget = new MSVSSGET();

    // Act
    msvssget.setQuiet(true);

    // Assert
    assertEquals(MSVSSConstants.FLAG_QUIET, msvssget.getQuiet());
  }

  /**
   * Test {@link MSVSSGET#setWritable(boolean)}.
   * <p>
   * Method under test: {@link MSVSSGET#setWritable(boolean)}
   */
  @Test
  public void testSetWritable() {
    // Arrange
    MSVSSGET msvssget = new MSVSSGET();

    // Act
    msvssget.setWritable(true);

    // Assert
    assertEquals(MSVSSConstants.FLAG_WRITABLE, msvssget.getWritable());
  }

  /**
   * Test {@link MSVSSGET#setVersion(String)}.
   * <p>
   * Method under test: {@link MSVSSGET#setVersion(String)}
   */
  @Test
  public void testSetVersion() {
    // Arrange
    MSVSSGET msvssget = new MSVSSGET();

    // Act
    msvssget.setVersion("1.0.2");

    // Assert
    assertEquals("-V1.0.2", msvssget.getVersion());
    assertEquals("-V1.0.2", msvssget.getVersionDateLabel());
  }

  /**
   * Test {@link MSVSSGET#setDate(String)}.
   * <p>
   * Method under test: {@link MSVSSGET#setDate(String)}
   */
  @Test
  public void testSetDate() {
    // Arrange
    MSVSSGET msvssget = new MSVSSGET();

    // Act
    msvssget.setDate("2020-03-01");

    // Assert
    assertEquals("-Vd2020-03-01", msvssget.getVersionDateLabel());
  }

  /**
   * Test {@link MSVSSGET#setLabel(String)}.
   * <p>
   * Method under test: {@link MSVSSGET#setLabel(String)}
   */
  @Test
  public void testSetLabel() {
    // Arrange
    MSVSSGET msvssget = new MSVSSGET();

    // Act
    msvssget.setLabel(MSVSSConstants.COMMAND_LABEL);

    // Assert
    assertEquals("-LLabel", msvssget.getLabel());
    assertEquals("-VLLabel", msvssget.getVersionDateLabel());
  }

  /**
   * Test new {@link MSVSSGET} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link MSVSSGET}
   */
  @Test
  public void testNewMsvssget() throws BuildException {
    // Arrange and Act
    MSVSSGET actualMsvssget = new MSVSSGET();

    // Assert
    assertEquals("", actualMsvssget.getFileTimeStamp());
    assertEquals("", actualMsvssget.getGetLocalCopy());
    assertEquals("", actualMsvssget.getLabel());
    assertEquals("", actualMsvssget.getLocalpath());
    assertEquals("", actualMsvssget.getLogin());
    assertEquals("", actualMsvssget.getOutput());
    assertEquals("", actualMsvssget.getQuiet());
    assertEquals("", actualMsvssget.getRecursive());
    assertEquals("", actualMsvssget.getStyle());
    assertEquals("", actualMsvssget.getUser());
    assertEquals("", actualMsvssget.getVersion());
    assertEquals("", actualMsvssget.getVersionDate());
    assertEquals("", actualMsvssget.getVersionDateLabel());
    assertEquals("", actualMsvssget.getVersionLabel());
    assertEquals("", actualMsvssget.getWritable());
    assertEquals("", actualMsvssget.getWritableFiles());
    assertEquals("-C-", actualMsvssget.getComment());
    assertNull(actualMsvssget.getDescription());
    assertNull(actualMsvssget.getTaskName());
    assertNull(actualMsvssget.getTaskType());
    assertNull(actualMsvssget.getVsspath());
    assertNull(actualMsvssget.getProject());
    assertNull(actualMsvssget.getOwningTarget());
    assertEquals(MSVSSConstants.FLAG_AUTORESPONSE_DEF, actualMsvssget.getAutoresponse());
    assertEquals(MSVSSConstants.SS_EXE, actualMsvssget.getSSCommand());
  }
}
