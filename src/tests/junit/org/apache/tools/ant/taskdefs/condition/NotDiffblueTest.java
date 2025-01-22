package org.apache.tools.ant.taskdefs.condition;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.taskdefs.Available;
import org.junit.Test;

public class NotDiffblueTest {
  /**
   * Test {@link Not#eval()}.
   * <ul>
   *   <li>Given {@link And} (default constructor) addNot {@link Not} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Not#eval()}
   */
  @Test
  public void testEval_givenAndAddNotNot_thenThrowBuildException() throws BuildException {
    // Arrange
    And a = new And();
    a.addNot(new Not());
    a.addAvailable(new Available());

    Not not = new Not();
    not.addAnd(a);

    // Act and Assert
    assertThrows(BuildException.class, () -> not.eval());
  }

  /**
   * Test {@link Not#eval()}.
   * <ul>
   *   <li>Given {@link Available} (default constructor) Classname is {@code At least one of (classname|file|resource) is required}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Not#eval()}
   */
  @Test
  public void testEval_givenAvailableClassnameIsAtLeastOneOfClassnameFileResourceIsRequired() throws BuildException {
    // Arrange
    Available a = new Available();
    a.setClassname("At least one of (classname|file|resource) is required");

    Not not = new Not();
    not.addAvailable(a);

    // Act and Assert
    assertTrue(not.eval());
  }

  /**
   * Test {@link Not#eval()}.
   * <ul>
   *   <li>Given {@link Not} (default constructor) addAnd {@link And} (default constructor).</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Not#eval()}
   */
  @Test
  public void testEval_givenNotAddAndAnd_thenReturnFalse() throws BuildException {
    // Arrange
    Not not = new Not();
    not.addAnd(new And());

    // Act and Assert
    assertFalse(not.eval());
  }

  /**
   * Test {@link Not#eval()}.
   * <ul>
   *   <li>Given {@link Not} (default constructor) addAvailable {@link Available} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Not#eval()}
   */
  @Test
  public void testEval_givenNotAddAvailableAvailable_thenThrowBuildException() throws BuildException {
    // Arrange
    Not not = new Not();
    not.addAvailable(new Available());
    not.addAvailable(new Available());

    // Act and Assert
    assertThrows(BuildException.class, () -> not.eval());
  }

  /**
   * Test {@link Not#eval()}.
   * <ul>
   *   <li>Given {@link Not} (default constructor) addNot {@link Not} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Not#eval()}
   */
  @Test
  public void testEval_givenNotAddNotNot_thenThrowBuildException() throws BuildException {
    // Arrange
    Not not = new Not();
    not.addNot(new Not());

    // Act and Assert
    assertThrows(BuildException.class, () -> not.eval());
  }

  /**
   * Test {@link Not#eval()}.
   * <ul>
   *   <li>Given {@link Not} (default constructor) addOr {@link Or} (default constructor).</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Not#eval()}
   */
  @Test
  public void testEval_givenNotAddOrOr_thenReturnTrue() throws BuildException {
    // Arrange
    Not not = new Not();
    not.addOr(new Or());

    // Act and Assert
    assertTrue(not.eval());
  }

  /**
   * Test {@link Not#eval()}.
   * <ul>
   *   <li>Given {@link Not} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Not#eval()}
   */
  @Test
  public void testEval_givenNot_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Not()).eval());
  }

  /**
   * Test new {@link Not} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Not}
   */
  @Test
  public void testNewNot() {
    // Arrange and Act
    Not actualNot = new Not();

    // Assert
    assertEquals("component", actualNot.getTaskName());
    Location location = actualNot.getLocation();
    assertNull(location.getFileName());
    assertNull(actualNot.getDescription());
    assertNull(actualNot.getProject());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
  }
}
