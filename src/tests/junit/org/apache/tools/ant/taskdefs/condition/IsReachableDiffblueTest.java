package org.apache.tools.ant.taskdefs.condition;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.junit.Test;

public class IsReachableDiffblueTest {
  /**
   * Test {@link IsReachable#eval()}.
   * <ul>
   *   <li>Given {@link IsReachable} (default constructor) Host is {@code 42}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IsReachable#eval()}
   */
  @Test
  public void testEval_givenIsReachableHostIs42_thenReturnFalse() throws BuildException {
    // Arrange
    IsReachable isReachable = new IsReachable();
    isReachable.setTimeout(0);
    isReachable.setUrl(null);
    isReachable.setHost("42");

    // Act and Assert
    assertFalse(isReachable.eval());
  }

  /**
   * Test {@link IsReachable#eval()}.
   * <ul>
   *   <li>Given {@link IsReachable} (default constructor) Host is empty string.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IsReachable#eval()}
   */
  @Test
  public void testEval_givenIsReachableHostIsEmptyString_thenThrowBuildException() throws BuildException {
    // Arrange
    IsReachable isReachable = new IsReachable();
    isReachable.setTimeout(0);
    isReachable.setUrl(null);
    isReachable.setHost("");

    // Act and Assert
    assertThrows(BuildException.class, () -> isReachable.eval());
  }

  /**
   * Test {@link IsReachable#eval()}.
   * <ul>
   *   <li>Given {@link IsReachable} (default constructor) Host is {@code null}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IsReachable#eval()}
   */
  @Test
  public void testEval_givenIsReachableHostIsNull_thenThrowBuildException() throws BuildException {
    // Arrange
    IsReachable isReachable = new IsReachable();
    isReachable.setTimeout(0);
    isReachable.setUrl("foo");
    isReachable.setHost(null);

    // Act and Assert
    assertThrows(BuildException.class, () -> isReachable.eval());
  }

  /**
   * Test {@link IsReachable#eval()}.
   * <ul>
   *   <li>Given {@link IsReachable} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IsReachable#eval()}
   */
  @Test
  public void testEval_givenIsReachableProjectIsProject_thenReturnFalse() throws BuildException {
    // Arrange
    IsReachable isReachable = new IsReachable();
    isReachable.setProject(new Project());
    isReachable.setTimeout(0);
    isReachable.setUrl(null);
    isReachable.setHost("42");

    // Act and Assert
    assertFalse(isReachable.eval());
  }

  /**
   * Test {@link IsReachable#eval()}.
   * <ul>
   *   <li>Given {@link IsReachable} (default constructor) Timeout is minus one.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IsReachable#eval()}
   */
  @Test
  public void testEval_givenIsReachableTimeoutIsMinusOne_thenThrowBuildException() throws BuildException {
    // Arrange
    IsReachable isReachable = new IsReachable();
    isReachable.setTimeout(-1);
    isReachable.setUrl(null);
    isReachable.setHost("foo");

    // Act and Assert
    assertThrows(BuildException.class, () -> isReachable.eval());
  }

  /**
   * Test {@link IsReachable#eval()}.
   * <ul>
   *   <li>Given {@link IsReachable} (default constructor) Url is {@code foo}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IsReachable#eval()}
   */
  @Test
  public void testEval_givenIsReachableUrlIsFoo_thenThrowBuildException() throws BuildException {
    // Arrange
    IsReachable isReachable = new IsReachable();
    isReachable.setTimeout(0);
    isReachable.setUrl("foo");
    isReachable.setHost("foo");

    // Act and Assert
    assertThrows(BuildException.class, () -> isReachable.eval());
  }

  /**
   * Test {@link IsReachable#eval()}.
   * <ul>
   *   <li>Given {@link IsReachable} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IsReachable#eval()}
   */
  @Test
  public void testEval_givenIsReachable_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new IsReachable()).eval());
  }

  /**
   * Test {@link IsReachable#eval()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IsReachable#eval()}
   */
  @Test
  public void testEval_givenProjectAddBuildListenerAntClassLoader_thenReturnFalse() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    IsReachable isReachable = new IsReachable();
    isReachable.setProject(project);
    isReachable.setTimeout(0);
    isReachable.setUrl(null);
    isReachable.setHost("42");

    // Act and Assert
    assertFalse(isReachable.eval());
  }

  /**
   * Test new {@link IsReachable} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link IsReachable}
   */
  @Test
  public void testNewIsReachable() {
    // Arrange and Act
    IsReachable actualIsReachable = new IsReachable();

    // Assert
    Location location = actualIsReachable.getLocation();
    assertNull(location.getFileName());
    assertNull(actualIsReachable.getDescription());
    assertNull(actualIsReachable.getProject());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
  }
}
