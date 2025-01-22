package org.apache.tools.ant.taskdefs.condition;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.nio.file.Paths;
import org.apache.tools.ant.BuildException;
import org.junit.Test;

public class FilesMatchDiffblueTest {
  /**
   * Test {@link FilesMatch#eval()}.
   * <p>
   * Method under test: {@link FilesMatch#eval()}
   */
  @Test
  public void testEval() throws BuildException {
    // Arrange
    FilesMatch filesMatch = new FilesMatch();
    filesMatch.setFile2(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    filesMatch.setFile1(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertFalse(filesMatch.eval());
  }

  /**
   * Test {@link FilesMatch#eval()}.
   * <p>
   * Method under test: {@link FilesMatch#eval()}
   */
  @Test
  public void testEval2() throws BuildException {
    // Arrange
    FilesMatch filesMatch = new FilesMatch();
    filesMatch.setFile2(Paths.get(System.getProperty("java.io.tmpdir"), "foo").toFile());
    filesMatch.setFile1(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertFalse(filesMatch.eval());
  }

  /**
   * Test {@link FilesMatch#eval()}.
   * <p>
   * Method under test: {@link FilesMatch#eval()}
   */
  @Test
  public void testEval3() throws BuildException {
    // Arrange
    FilesMatch filesMatch = new FilesMatch();
    filesMatch.setFile2(
        Paths.get(System.getProperty("java.io.tmpdir"), "both file1 and file2 are required in filesmatch", "foo")
            .toFile());
    filesMatch.setFile1(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertFalse(filesMatch.eval());
  }

  /**
   * Test {@link FilesMatch#eval()}.
   * <ul>
   *   <li>Given {@link FilesMatch} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FilesMatch#eval()}
   */
  @Test
  public void testEval_givenFilesMatch_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new FilesMatch()).eval());
  }

  /**
   * Test {@link FilesMatch#eval()}.
   * <ul>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FilesMatch#eval()}
   */
  @Test
  public void testEval_thenReturnTrue() throws BuildException {
    // Arrange
    FilesMatch filesMatch = new FilesMatch();
    filesMatch.setFile2(
        Paths.get(System.getProperty("java.io.tmpdir"), "both file1 and file2 are required in filesmatch", "foo")
            .toFile());
    filesMatch.setFile1(
        Paths.get(System.getProperty("java.io.tmpdir"), "both file1 and file2 are required in filesmatch", "foo")
            .toFile());

    // Act and Assert
    assertTrue(filesMatch.eval());
  }

  /**
   * Test {@link FilesMatch#eval()}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FilesMatch#eval()}
   */
  @Test
  public void testEval_thenThrowBuildException() throws BuildException {
    // Arrange
    FilesMatch filesMatch = new FilesMatch();
    filesMatch.setFile1(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> filesMatch.eval());
  }
}
