package org.apache.tools.ant.taskdefs.condition;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.taskdefs.Available;
import org.junit.Test;

public class XorDiffblueTest {
  /**
   * Test {@link Xor#eval()}.
   * <ul>
   *   <li>Given {@link Available} (default constructor) Classname is {@code At least one of (classname|file|resource) is required}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Xor#eval()}
   */
  @Test
  public void testEval_givenAvailableClassnameIsAtLeastOneOfClassnameFileResourceIsRequired() throws BuildException {
    // Arrange
    Available a = new Available();
    a.setClassname("At least one of (classname|file|resource) is required");

    Xor xor = new Xor();
    xor.addAvailable(a);

    // Act and Assert
    assertFalse(xor.eval());
  }

  /**
   * Test {@link Xor#eval()}.
   * <ul>
   *   <li>Given {@link Xor} (default constructor) addAnd {@link And} (default constructor).</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Xor#eval()}
   */
  @Test
  public void testEval_givenXorAddAndAnd_thenReturnTrue() throws BuildException {
    // Arrange
    Xor xor = new Xor();
    xor.addAnd(new And());

    // Act and Assert
    assertTrue(xor.eval());
  }

  /**
   * Test {@link Xor#eval()}.
   * <ul>
   *   <li>Given {@link Xor} (default constructor).</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Xor#eval()}
   */
  @Test
  public void testEval_givenXor_thenReturnFalse() throws BuildException {
    // Arrange, Act and Assert
    assertFalse((new Xor()).eval());
  }

  /**
   * Test new {@link Xor} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Xor}
   */
  @Test
  public void testNewXor() {
    // Arrange and Act
    Xor actualXor = new Xor();

    // Assert
    assertEquals("component", actualXor.getTaskName());
    Location location = actualXor.getLocation();
    assertNull(location.getFileName());
    assertNull(actualXor.getDescription());
    assertNull(actualXor.getProject());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
  }
}
