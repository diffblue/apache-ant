package org.apache.tools.ant.taskdefs.condition;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.junit.Test;

public class IsFalseDiffblueTest {
  /**
   * Test {@link IsFalse#eval()}.
   * <ul>
   *   <li>Given {@link IsFalse} (default constructor) Value is {@code false}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IsFalse#eval()}
   */
  @Test
  public void testEval_givenIsFalseValueIsFalse_thenReturnTrue() throws BuildException {
    // Arrange
    IsFalse isFalse = new IsFalse();
    isFalse.setValue(false);

    // Act and Assert
    assertTrue(isFalse.eval());
  }

  /**
   * Test {@link IsFalse#eval()}.
   * <ul>
   *   <li>Given {@link IsFalse} (default constructor) Value is {@code true}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IsFalse#eval()}
   */
  @Test
  public void testEval_givenIsFalseValueIsTrue_thenReturnFalse() throws BuildException {
    // Arrange
    IsFalse isFalse = new IsFalse();
    isFalse.setValue(true);

    // Act and Assert
    assertFalse(isFalse.eval());
  }

  /**
   * Test {@link IsFalse#eval()}.
   * <ul>
   *   <li>Given {@link IsFalse} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IsFalse#eval()}
   */
  @Test
  public void testEval_givenIsFalse_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new IsFalse()).eval());
  }

  /**
   * Test new {@link IsFalse} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link IsFalse}
   */
  @Test
  public void testNewIsFalse() {
    // Arrange and Act
    IsFalse actualIsFalse = new IsFalse();

    // Assert
    Location location = actualIsFalse.getLocation();
    assertNull(location.getFileName());
    assertNull(actualIsFalse.getDescription());
    assertNull(actualIsFalse.getProject());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
  }
}
