package org.apache.tools.ant.types.optional.imageio;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertSame;
import java.util.List;
import org.junit.Test;

public class ImageOperationDiffblueTest {
  /**
   * Test {@link ImageOperation#addRotate(Rotate)}.
   * <p>
   * Method under test: {@link ImageOperation#addRotate(Rotate)}
   */
  @Test
  public void testAddRotate() {
    // Arrange
    Arc arc = new Arc();
    Rotate instr = new Rotate();

    // Act
    arc.addRotate(instr);

    // Assert
    List<ImageOperation> imageOperationList = arc.instructions;
    assertEquals(1, imageOperationList.size());
    assertSame(instr, imageOperationList.get(0));
  }

  /**
   * Test {@link ImageOperation#addDraw(Draw)}.
   * <p>
   * Method under test: {@link ImageOperation#addDraw(Draw)}
   */
  @Test
  public void testAddDraw() {
    // Arrange
    Arc arc = new Arc();
    Draw instr = new Draw();

    // Act
    arc.addDraw(instr);

    // Assert
    List<ImageOperation> imageOperationList = arc.instructions;
    assertEquals(1, imageOperationList.size());
    assertSame(instr, imageOperationList.get(0));
  }

  /**
   * Test {@link ImageOperation#addScale(Scale)}.
   * <p>
   * Method under test: {@link ImageOperation#addScale(Scale)}
   */
  @Test
  public void testAddScale() {
    // Arrange
    Arc arc = new Arc();
    Scale instr = new Scale();

    // Act
    arc.addScale(instr);

    // Assert
    List<ImageOperation> imageOperationList = arc.instructions;
    assertEquals(1, imageOperationList.size());
    assertSame(instr, imageOperationList.get(0));
  }
}
