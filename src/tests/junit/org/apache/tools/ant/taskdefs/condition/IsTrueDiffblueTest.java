package org.apache.tools.ant.taskdefs.condition;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.junit.Test;

public class IsTrueDiffblueTest {
  /**
   * Test {@link IsTrue#eval()}.
   * <ul>
   *   <li>Given {@link IsTrue} (default constructor) Value is {@code false}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IsTrue#eval()}
   */
  @Test
  public void testEval_givenIsTrueValueIsFalse_thenReturnFalse() throws BuildException {
    // Arrange
    IsTrue isTrue = new IsTrue();
    isTrue.setValue(false);

    // Act and Assert
    assertFalse(isTrue.eval());
  }

  /**
   * Test {@link IsTrue#eval()}.
   * <ul>
   *   <li>Given {@link IsTrue} (default constructor) Value is {@code true}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IsTrue#eval()}
   */
  @Test
  public void testEval_givenIsTrueValueIsTrue_thenReturnTrue() throws BuildException {
    // Arrange
    IsTrue isTrue = new IsTrue();
    isTrue.setValue(true);

    // Act and Assert
    assertTrue(isTrue.eval());
  }

  /**
   * Test {@link IsTrue#eval()}.
   * <ul>
   *   <li>Given {@link IsTrue} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IsTrue#eval()}
   */
  @Test
  public void testEval_givenIsTrue_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new IsTrue()).eval());
  }

  /**
   * Test new {@link IsTrue} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link IsTrue}
   */
  @Test
  public void testNewIsTrue() {
    // Arrange and Act
    IsTrue actualIsTrue = new IsTrue();

    // Assert
    Location location = actualIsTrue.getLocation();
    assertNull(location.getFileName());
    assertNull(actualIsTrue.getDescription());
    assertNull(actualIsTrue.getProject());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
  }
}
