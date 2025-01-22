package org.apache.tools.ant.taskdefs.optional.vss;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import java.io.File;
import java.nio.file.Paths;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.taskdefs.optional.vss.MSVSSHISTORY.BriefCodediffNofile;
import org.junit.Test;

public class MSVSSHISTORYDiffblueTest {
  /**
   * Test BriefCodediffNofile {@link BriefCodediffNofile#getValues()}.
   * <p>
   * Method under test: {@link BriefCodediffNofile#getValues()}
   */
  @Test
  public void testBriefCodediffNofileGetValues() {
    // Arrange, Act and Assert
    assertArrayEquals(new String[]{MSVSSConstants.STYLE_BRIEF, MSVSSConstants.STYLE_CODEDIFF,
        MSVSSConstants.STYLE_NOFILE, MSVSSConstants.STYLE_DEFAULT}, (new BriefCodediffNofile()).getValues());
  }

  /**
   * Test BriefCodediffNofile new {@link BriefCodediffNofile} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link BriefCodediffNofile}
   */
  @Test
  public void testBriefCodediffNofileNewBriefCodediffNofile() {
    // Arrange and Act
    BriefCodediffNofile actualBriefCodediffNofile = new BriefCodediffNofile();

    // Assert
    assertNull(actualBriefCodediffNofile.getValue());
    assertEquals(-1, actualBriefCodediffNofile.getIndex());
  }

  /**
   * Test {@link MSVSSHISTORY#buildCmdLine()}.
   * <ul>
   *   <li>Given {@link MSVSSHISTORY} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MSVSSHISTORY#buildCmdLine()}
   */
  @Test
  public void testBuildCmdLine_givenMsvsshistory_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new MSVSSHISTORY()).buildCmdLine());
  }

  /**
   * Test {@link MSVSSHISTORY#setRecursive(boolean)}.
   * <p>
   * Method under test: {@link MSVSSHISTORY#setRecursive(boolean)}
   */
  @Test
  public void testSetRecursive() {
    // Arrange
    MSVSSHISTORY msvsshistory = new MSVSSHISTORY();

    // Act
    msvsshistory.setRecursive(true);

    // Assert
    assertEquals(MSVSSConstants.FLAG_RECURSION, msvsshistory.getRecursive());
  }

  /**
   * Test {@link MSVSSHISTORY#setFromDate(String)}.
   * <p>
   * Method under test: {@link MSVSSHISTORY#setFromDate(String)}
   */
  @Test
  public void testSetFromDate() throws BuildException {
    // Arrange
    MSVSSHISTORY msvsshistory = new MSVSSHISTORY();

    // Act
    msvsshistory.setFromDate("2020-03-01");

    // Assert
    assertEquals("-V~d2020-03-01", msvsshistory.getVersionDate());
  }

  /**
   * Test {@link MSVSSHISTORY#setFromLabel(String)}.
   * <p>
   * Method under test: {@link MSVSSHISTORY#setFromLabel(String)}
   */
  @Test
  public void testSetFromLabel() {
    // Arrange
    MSVSSHISTORY msvsshistory = new MSVSSHISTORY();

    // Act
    msvsshistory.setFromLabel("jane.doe@example.org");

    // Assert
    assertEquals("-V~Ljane.doe@example.org", msvsshistory.getVersionLabel());
  }

  /**
   * Test {@link MSVSSHISTORY#setNumdays(int)}.
   * <p>
   * Method under test: {@link MSVSSHISTORY#setNumdays(int)}
   */
  @Test
  public void testSetNumdays() throws BuildException {
    // Arrange
    MSVSSHISTORY msvsshistory = new MSVSSHISTORY();

    // Act
    msvsshistory.setNumdays(10);

    // Assert
    assertEquals("-Vdnull", msvsshistory.getVersionDate());
  }

  /**
   * Test {@link MSVSSHISTORY#setOutput(File)}.
   * <p>
   * Method under test: {@link MSVSSHISTORY#setOutput(File)}
   */
  @Test
  public void testSetOutput() {
    // Arrange
    MSVSSHISTORY msvsshistory = new MSVSSHISTORY();

    // Act
    msvsshistory.setOutput(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Assert
    String expectedOutput = String.join("", MSVSSConstants.FLAG_OUTPUT,
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toString());
    assertEquals(expectedOutput, msvsshistory.getOutput());
  }

  /**
   * Test {@link MSVSSHISTORY#setOutput(File)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then {@link MSVSSHISTORY} (default constructor) Output is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link MSVSSHISTORY#setOutput(File)}
   */
  @Test
  public void testSetOutput_whenNull_thenMsvsshistoryOutputIsEmptyString() {
    // Arrange
    MSVSSHISTORY msvsshistory = new MSVSSHISTORY();

    // Act
    msvsshistory.setOutput(null);

    // Assert that nothing has changed
    assertEquals("", msvsshistory.getOutput());
  }

  /**
   * Test new {@link MSVSSHISTORY} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link MSVSSHISTORY}
   */
  @Test
  public void testNewMsvsshistory() throws BuildException {
    // Arrange and Act
    MSVSSHISTORY actualMsvsshistory = new MSVSSHISTORY();

    // Assert
    assertEquals("", actualMsvsshistory.getFileTimeStamp());
    assertEquals("", actualMsvsshistory.getGetLocalCopy());
    assertEquals("", actualMsvsshistory.getLabel());
    assertEquals("", actualMsvsshistory.getLocalpath());
    assertEquals("", actualMsvsshistory.getLogin());
    assertEquals("", actualMsvsshistory.getOutput());
    assertEquals("", actualMsvsshistory.getQuiet());
    assertEquals("", actualMsvsshistory.getRecursive());
    assertEquals("", actualMsvsshistory.getStyle());
    assertEquals("", actualMsvsshistory.getUser());
    assertEquals("", actualMsvsshistory.getVersion());
    assertEquals("", actualMsvsshistory.getVersionDate());
    assertEquals("", actualMsvsshistory.getVersionDateLabel());
    assertEquals("", actualMsvsshistory.getVersionLabel());
    assertEquals("", actualMsvsshistory.getWritable());
    assertEquals("", actualMsvsshistory.getWritableFiles());
    assertEquals("-C-", actualMsvsshistory.getComment());
    assertNull(actualMsvsshistory.getDescription());
    assertNull(actualMsvsshistory.getTaskName());
    assertNull(actualMsvsshistory.getTaskType());
    assertNull(actualMsvsshistory.getVsspath());
    assertNull(actualMsvsshistory.getProject());
    assertNull(actualMsvsshistory.getOwningTarget());
    assertEquals(MSVSSConstants.FLAG_AUTORESPONSE_DEF, actualMsvsshistory.getAutoresponse());
    assertEquals(MSVSSConstants.SS_EXE, actualMsvsshistory.getSSCommand());
  }
}
