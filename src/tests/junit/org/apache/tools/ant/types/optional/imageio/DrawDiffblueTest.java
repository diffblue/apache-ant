package org.apache.tools.ant.types.optional.imageio;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import java.awt.image.BufferedImage;
import java.awt.image.ColorModel;
import java.awt.image.DirectColorModel;
import java.awt.image.SampleModel;
import java.awt.image.SinglePixelPackedSampleModel;
import java.util.List;
import org.apache.tools.ant.Location;
import org.junit.Test;

public class DrawDiffblueTest {
  /**
   * Test {@link Draw#addText(Text)}.
   * <p>
   * Method under test: {@link Draw#addText(Text)}
   */
  @Test
  public void testAddText() {
    // Arrange
    Draw draw = new Draw();
    Text text = new Text();

    // Act
    draw.addText(text);

    // Assert
    List<ImageOperation> imageOperationList = draw.instructions;
    assertEquals(1, imageOperationList.size());
    assertSame(text, imageOperationList.get(0));
  }

  /**
   * Test {@link Draw#addRectangle(Rectangle)}.
   * <p>
   * Method under test: {@link Draw#addRectangle(Rectangle)}
   */
  @Test
  public void testAddRectangle() {
    // Arrange
    Draw draw = new Draw();
    Rectangle rect = new Rectangle();

    // Act
    draw.addRectangle(rect);

    // Assert
    List<ImageOperation> imageOperationList = draw.instructions;
    assertEquals(1, imageOperationList.size());
    assertSame(rect, imageOperationList.get(0));
  }

  /**
   * Test {@link Draw#addEllipse(Ellipse)}.
   * <p>
   * Method under test: {@link Draw#addEllipse(Ellipse)}
   */
  @Test
  public void testAddEllipse() {
    // Arrange
    Draw draw = new Draw();
    Ellipse elip = new Ellipse();

    // Act
    draw.addEllipse(elip);

    // Assert
    List<ImageOperation> imageOperationList = draw.instructions;
    assertEquals(1, imageOperationList.size());
    assertSame(elip, imageOperationList.get(0));
  }

  /**
   * Test {@link Draw#addArc(Arc)}.
   * <p>
   * Method under test: {@link Draw#addArc(Arc)}
   */
  @Test
  public void testAddArc() {
    // Arrange
    Draw draw = new Draw();
    Arc arc = new Arc();

    // Act
    draw.addArc(arc);

    // Assert
    List<ImageOperation> imageOperationList = draw.instructions;
    assertEquals(1, imageOperationList.size());
    assertSame(arc, imageOperationList.get(0));
  }

  /**
   * Test {@link Draw#executeTransformOperation(BufferedImage)}.
   * <ul>
   *   <li>Given {@link Draw} (default constructor) addText {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Draw#executeTransformOperation(BufferedImage)}
   */
  @Test
  public void testExecuteTransformOperation_givenDrawAddTextNull() {
    // Arrange
    Draw draw = new Draw();
    draw.addText(null);

    // Act
    BufferedImage actualExecuteTransformOperationResult = draw.executeTransformOperation(new BufferedImage(1, 1, 1));

    // Assert
    ColorModel colorModel = actualExecuteTransformOperationResult.getColorModel();
    assertTrue(colorModel instanceof DirectColorModel);
    SampleModel sampleModel = actualExecuteTransformOperationResult.getSampleModel();
    assertTrue(sampleModel instanceof SinglePixelPackedSampleModel);
    assertArrayEquals(new int[]{16711680, 65280, 255}, ((DirectColorModel) colorModel).getMasks());
    assertArrayEquals(new int[]{16711680, 65280, 255}, ((SinglePixelPackedSampleModel) sampleModel).getBitMasks());
    assertArrayEquals(new int[]{8, 8, 8}, colorModel.getComponentSize());
    assertArrayEquals(new int[]{8, 8, 8}, sampleModel.getSampleSize());
    assertArrayEquals(new int[]{Short.SIZE, 8, 0}, ((SinglePixelPackedSampleModel) sampleModel).getBitOffsets());
  }

  /**
   * Test {@link Draw#executeTransformOperation(BufferedImage)}.
   * <ul>
   *   <li>Given {@link Draw} (default constructor).</li>
   *   <li>Then ColorModel return {@link DirectColorModel}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Draw#executeTransformOperation(BufferedImage)}
   */
  @Test
  public void testExecuteTransformOperation_givenDraw_thenColorModelReturnDirectColorModel() {
    // Arrange
    Draw draw = new Draw();

    // Act
    BufferedImage actualExecuteTransformOperationResult = draw.executeTransformOperation(new BufferedImage(1, 1, 1));

    // Assert
    ColorModel colorModel = actualExecuteTransformOperationResult.getColorModel();
    assertTrue(colorModel instanceof DirectColorModel);
    SampleModel sampleModel = actualExecuteTransformOperationResult.getSampleModel();
    assertTrue(sampleModel instanceof SinglePixelPackedSampleModel);
    assertArrayEquals(new int[]{16711680, 65280, 255}, ((DirectColorModel) colorModel).getMasks());
    assertArrayEquals(new int[]{16711680, 65280, 255}, ((SinglePixelPackedSampleModel) sampleModel).getBitMasks());
    assertArrayEquals(new int[]{8, 8, 8}, colorModel.getComponentSize());
    assertArrayEquals(new int[]{8, 8, 8}, sampleModel.getSampleSize());
    assertArrayEquals(new int[]{Short.SIZE, 8, 0}, ((SinglePixelPackedSampleModel) sampleModel).getBitOffsets());
  }

  /**
   * Test new {@link Draw} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Draw}
   */
  @Test
  public void testNewDraw() {
    // Arrange and Act
    Draw actualDraw = new Draw();

    // Assert
    Location location = actualDraw.getLocation();
    assertNull(location.getFileName());
    assertNull(actualDraw.getDescription());
    assertNull(actualDraw.getProject());
    assertNull(actualDraw.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(actualDraw.isReference());
    assertTrue(actualDraw.instructions.isEmpty());
  }
}
