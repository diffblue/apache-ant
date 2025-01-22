package org.apache.tools.ant.types.optional.imageio;

import static org.junit.Assert.assertEquals;
import org.junit.Test;

public class BasicShapeDiffblueTest {
  /**
   * Test {@link BasicShape#setWidth(int)}.
   * <p>
   * Method under test: {@link BasicShape#setWidth(int)}
   */
  @Test
  public void testSetWidth() {
    // Arrange
    Arc arc = new Arc();

    // Act
    arc.setWidth(1);

    // Assert
    assertEquals(1, arc.width);
  }

  /**
   * Test {@link BasicShape#setHeight(int)}.
   * <p>
   * Method under test: {@link BasicShape#setHeight(int)}
   */
  @Test
  public void testSetHeight() {
    // Arrange
    Arc arc = new Arc();

    // Act
    arc.setHeight(1);

    // Assert
    assertEquals(1, arc.height);
  }

  /**
   * Test {@link BasicShape#setStrokewidth(int)}.
   * <p>
   * Method under test: {@link BasicShape#setStrokewidth(int)}
   */
  @Test
  public void testSetStrokewidth() {
    // Arrange
    Arc arc = new Arc();

    // Act
    arc.setStrokewidth(1);

    // Assert
    assertEquals(1, arc.strokeWidth);
  }

  /**
   * Test {@link BasicShape#setStroke(String)}.
   * <p>
   * Method under test: {@link BasicShape#setStroke(String)}
   */
  @Test
  public void testSetStroke() {
    // Arrange
    Arc arc = new Arc();

    // Act
    arc.setStroke("Col");

    // Assert
    assertEquals("Col", arc.stroke);
  }

  /**
   * Test {@link BasicShape#setFill(String)}.
   * <p>
   * Method under test: {@link BasicShape#setFill(String)}
   */
  @Test
  public void testSetFill() {
    // Arrange
    Arc arc = new Arc();

    // Act
    arc.setFill("Col");

    // Assert
    assertEquals("Col", arc.fill);
  }
}
