package org.apache.tools.ant.taskdefs.condition;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.Target;
import org.junit.Test;

public class IsSetDiffblueTest {
  /**
   * Test {@link IsSet#eval()}.
   * <ul>
   *   <li>Given {@link IsSet} (default constructor) Property is {@code ant.refid:}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IsSet#eval()}
   */
  @Test
  public void testEval_givenIsSetPropertyIsAntRefid_thenReturnFalse() throws BuildException {
    // Arrange
    IsSet isSet = new IsSet();
    isSet.setProperty("ant.refid:");
    isSet.setProject(new Project());

    // Act and Assert
    assertFalse(isSet.eval());
  }

  /**
   * Test {@link IsSet#eval()}.
   * <ul>
   *   <li>Given {@link IsSet} (default constructor) Property is {@code foo}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IsSet#eval()}
   */
  @Test
  public void testEval_givenIsSetPropertyIsFoo_thenReturnFalse() throws BuildException {
    // Arrange
    IsSet isSet = new IsSet();
    isSet.setProperty("foo");
    isSet.setProject(new Project());

    // Act and Assert
    assertFalse(isSet.eval());
  }

  /**
   * Test {@link IsSet#eval()}.
   * <ul>
   *   <li>Given {@link IsSet} (default constructor) Property is {@code toString:}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IsSet#eval()}
   */
  @Test
  public void testEval_givenIsSetPropertyIsToString_thenReturnFalse() throws BuildException {
    // Arrange
    IsSet isSet = new IsSet();
    isSet.setProperty("toString:");
    isSet.setProject(new Project());

    // Act and Assert
    assertFalse(isSet.eval());
  }

  /**
   * Test {@link IsSet#eval()}.
   * <ul>
   *   <li>Given {@link IsSet} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IsSet#eval()}
   */
  @Test
  public void testEval_givenIsSet_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new IsSet()).eval());
  }

  /**
   * Test {@link IsSet#eval()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IsSet#eval()}
   */
  @Test
  public void testEval_givenProjectAddBuildListenerAntClassLoader_thenReturnFalse() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    IsSet isSet = new IsSet();
    isSet.setProperty("foo");
    isSet.setProject(project);

    // Act and Assert
    assertFalse(isSet.eval());
  }

  /**
   * Test {@link IsSet#eval()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addTarget {@code Adding reference:} and {@link Target#Target()}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IsSet#eval()}
   */
  @Test
  public void testEval_givenProjectAddTargetAddingReferenceAndTarget_thenReturnFalse() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addTarget("Adding reference: ", new Target());
    project.addBuildListener(new AntClassLoader());

    IsSet isSet = new IsSet();
    isSet.setProperty("foo");
    isSet.setProject(project);

    // Act and Assert
    assertFalse(isSet.eval());
  }

  /**
   * Test new {@link IsSet} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link IsSet}
   */
  @Test
  public void testNewIsSet() {
    // Arrange and Act
    IsSet actualIsSet = new IsSet();

    // Assert
    Location location = actualIsSet.getLocation();
    assertNull(location.getFileName());
    assertNull(actualIsSet.getDescription());
    assertNull(actualIsSet.getProject());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
  }
}
