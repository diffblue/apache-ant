package org.apache.tools.ant.taskdefs.condition;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.taskdefs.Available;
import org.junit.Test;

public class AndDiffblueTest {
  /**
   * Test {@link And#eval()}.
   * <ul>
   *   <li>Given {@link And} (default constructor) addOr {@link Or} (default constructor).</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link And#eval()}
   */
  @Test
  public void testEval_givenAndAddOrOr_thenReturnFalse() throws BuildException {
    // Arrange
    And and = new And();
    and.addOr(new Or());
    and.addAvailable(new Available());

    // Act and Assert
    assertFalse(and.eval());
  }

  /**
   * Test {@link And#eval()}.
   * <ul>
   *   <li>Given {@link And} (default constructor).</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link And#eval()}
   */
  @Test
  public void testEval_givenAnd_thenReturnTrue() throws BuildException {
    // Arrange, Act and Assert
    assertTrue((new And()).eval());
  }

  /**
   * Test {@link And#eval()}.
   * <ul>
   *   <li>Given {@link Available} (default constructor) Classname is {@code At least one of (classname|file|resource) is required}.</li>
   * </ul>
   * <p>
   * Method under test: {@link And#eval()}
   */
  @Test
  public void testEval_givenAvailableClassnameIsAtLeastOneOfClassnameFileResourceIsRequired() throws BuildException {
    // Arrange
    Available a = new Available();
    a.setClassname("At least one of (classname|file|resource) is required");

    And and = new And();
    and.addAvailable(a);

    // Act and Assert
    assertFalse(and.eval());
  }

  /**
   * Test {@link And#eval()}.
   * <ul>
   *   <li>Given {@link Not} (default constructor) addAnd {@link And} (default constructor).</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link And#eval()}
   */
  @Test
  public void testEval_givenNotAddAndAnd_thenReturnFalse() throws BuildException {
    // Arrange
    Not n = new Not();
    n.addAnd(new And());

    And and = new And();
    and.addNot(n);
    and.addAvailable(new Available());

    // Act and Assert
    assertFalse(and.eval());
  }

  /**
   * Test new {@link And} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link And}
   */
  @Test
  public void testNewAnd() {
    // Arrange and Act
    And actualAnd = new And();

    // Assert
    assertEquals("component", actualAnd.getTaskName());
    Location location = actualAnd.getLocation();
    assertNull(location.getFileName());
    assertNull(actualAnd.getDescription());
    assertNull(actualAnd.getProject());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
  }
}
