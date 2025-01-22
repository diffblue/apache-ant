package org.apache.tools.ant.taskdefs.condition;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.taskdefs.Available;
import org.junit.Test;

public class OrDiffblueTest {
  /**
   * Test {@link Or#eval()}.
   * <ul>
   *   <li>Given {@link Available} (default constructor) Classname is {@code At least one of (classname|file|resource) is required}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Or#eval()}
   */
  @Test
  public void testEval_givenAvailableClassnameIsAtLeastOneOfClassnameFileResourceIsRequired() throws BuildException {
    // Arrange
    Available a = new Available();
    a.setClassname("At least one of (classname|file|resource) is required");

    Or or = new Or();
    or.addAvailable(a);

    // Act and Assert
    assertFalse(or.eval());
  }

  /**
   * Test {@link Or#eval()}.
   * <ul>
   *   <li>Given {@link Not} (default constructor) addOr {@link Or} (default constructor).</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Or#eval()}
   */
  @Test
  public void testEval_givenNotAddOrOr_thenReturnTrue() throws BuildException {
    // Arrange
    Not n = new Not();
    n.addOr(new Or());

    Or or = new Or();
    or.addNot(n);
    or.addAvailable(new Available());

    // Act and Assert
    assertTrue(or.eval());
  }

  /**
   * Test {@link Or#eval()}.
   * <ul>
   *   <li>Given {@link Or} (default constructor) addAnd {@link And} (default constructor).</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Or#eval()}
   */
  @Test
  public void testEval_givenOrAddAndAnd_thenReturnTrue() throws BuildException {
    // Arrange
    Or or = new Or();
    or.addAnd(new And());
    or.addAvailable(new Available());

    // Act and Assert
    assertTrue(or.eval());
  }

  /**
   * Test {@link Or#eval()}.
   * <ul>
   *   <li>Given {@link Or} (default constructor).</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Or#eval()}
   */
  @Test
  public void testEval_givenOr_thenReturnFalse() throws BuildException {
    // Arrange, Act and Assert
    assertFalse((new Or()).eval());
  }

  /**
   * Test new {@link Or} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Or}
   */
  @Test
  public void testNewOr() {
    // Arrange and Act
    Or actualOr = new Or();

    // Assert
    assertEquals("component", actualOr.getTaskName());
    Location location = actualOr.getLocation();
    assertNull(location.getFileName());
    assertNull(actualOr.getDescription());
    assertNull(actualOr.getProject());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
  }
}
