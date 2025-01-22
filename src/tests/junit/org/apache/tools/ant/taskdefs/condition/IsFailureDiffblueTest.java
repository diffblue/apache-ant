package org.apache.tools.ant.taskdefs.condition;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import org.junit.Test;

public class IsFailureDiffblueTest {
  /**
   * Test {@link IsFailure#eval()}.
   * <ul>
   *   <li>Given {@link IsFailure} (default constructor) Code is one.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IsFailure#eval()}
   */
  @Test
  public void testEval_givenIsFailureCodeIsOne_thenReturnTrue() {
    // Arrange
    IsFailure isFailure = new IsFailure();
    isFailure.setCode(1);

    // Act and Assert
    assertTrue(isFailure.eval());
  }

  /**
   * Test {@link IsFailure#eval()}.
   * <ul>
   *   <li>Given {@link IsFailure} (default constructor) Code is zero.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IsFailure#eval()}
   */
  @Test
  public void testEval_givenIsFailureCodeIsZero_thenReturnFalse() {
    // Arrange
    IsFailure isFailure = new IsFailure();
    isFailure.setCode(0);

    // Act and Assert
    assertFalse(isFailure.eval());
  }

  /**
   * Test {@link IsFailure#eval()}.
   * <ul>
   *   <li>Given {@link IsFailure} (default constructor).</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IsFailure#eval()}
   */
  @Test
  public void testEval_givenIsFailure_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new IsFailure()).eval());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link IsFailure}
   *   <li>{@link IsFailure#setCode(int)}
   *   <li>{@link IsFailure#getCode()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    IsFailure actualIsFailure = new IsFailure();
    actualIsFailure.setCode(1);

    // Assert
    assertEquals(1, actualIsFailure.getCode());
  }
}
