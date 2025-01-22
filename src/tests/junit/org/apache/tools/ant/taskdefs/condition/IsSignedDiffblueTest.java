package org.apache.tools.ant.taskdefs.condition;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import java.nio.file.Paths;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.junit.Test;

public class IsSignedDiffblueTest {
  /**
   * Test {@link IsSigned#eval()}.
   * <p>
   * Method under test: {@link IsSigned#eval()}
   */
  @Test
  public void testEval() {
    // Arrange
    IsSigned isSigned = new IsSigned();
    isSigned.setFile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    isSigned.setName(null);

    // Act and Assert
    assertFalse(isSigned.eval());
  }

  /**
   * Test {@link IsSigned#eval()}.
   * <p>
   * Method under test: {@link IsSigned#eval()}
   */
  @Test
  public void testEval2() {
    // Arrange
    IsSigned isSigned = new IsSigned();
    isSigned.setFile(Paths.get(System.getProperty("java.io.tmpdir"), "file.encoding").toFile());
    isSigned.setName(null);

    // Act and Assert
    assertFalse(isSigned.eval());
  }

  /**
   * Test {@link IsSigned#eval()}.
   * <ul>
   *   <li>Given {@link IsSigned} (default constructor) Name is {@code foo}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IsSigned#eval()}
   */
  @Test
  public void testEval_givenIsSignedNameIsFoo_thenReturnFalse() {
    // Arrange
    IsSigned isSigned = new IsSigned();
    isSigned.setFile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    isSigned.setName("foo");

    // Act and Assert
    assertFalse(isSigned.eval());
  }

  /**
   * Test {@link IsSigned#eval()}.
   * <ul>
   *   <li>Given {@link IsSigned} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IsSigned#eval()}
   */
  @Test
  public void testEval_givenIsSignedProjectIsProject_thenReturnFalse() {
    // Arrange
    IsSigned isSigned = new IsSigned();
    isSigned.setProject(new Project());
    isSigned.setFile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    isSigned.setName(null);

    // Act and Assert
    assertFalse(isSigned.eval());
  }

  /**
   * Test {@link IsSigned#eval()}.
   * <ul>
   *   <li>Given {@link IsSigned} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IsSigned#eval()}
   */
  @Test
  public void testEval_givenIsSigned_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new IsSigned()).eval());
  }

  /**
   * Test new {@link IsSigned} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link IsSigned}
   */
  @Test
  public void testNewIsSigned() {
    // Arrange and Act
    IsSigned actualIsSigned = new IsSigned();

    // Assert
    Location location = actualIsSigned.getLocation();
    assertNull(location.getFileName());
    assertNull(actualIsSigned.getDescription());
    assertNull(actualIsSigned.getProject());
    assertNull(actualIsSigned.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(actualIsSigned.isReference());
  }
}
