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
import org.apache.tools.ant.Location;
import org.junit.Test;

public class RotateDiffblueTest {
  /**
   * Test {@link Rotate#performRotate(BufferedImage)}.
   * <ul>
   *   <li>Given {@link Rotate} (default constructor).</li>
   *   <li>Then return {@link BufferedImage#BufferedImage(int, int, int)} with one and one and one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rotate#performRotate(BufferedImage)}
   */
  @Test
  public void testPerformRotate_givenRotate_thenReturnBufferedImageWithOneAndOneAndOne() {
    // Arrange
    Rotate rotate = new Rotate();
    BufferedImage image = new BufferedImage(1, 1, 1);

    // Act and Assert
    assertSame(image, rotate.performRotate(image));
  }

  /**
   * Test {@link Rotate#performRotate(BufferedImage)}.
   * <ul>
   *   <li>When {@link BufferedImage#BufferedImage(int, int, int)} with four and one and one.</li>
   *   <li>Then return Height is two.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rotate#performRotate(BufferedImage)}
   */
  @Test
  public void testPerformRotate_whenBufferedImageWithFourAndOneAndOne_thenReturnHeightIsTwo() {
    // Arrange
    Rotate rotate = new Rotate();
    rotate.setAngle("42");

    // Act
    BufferedImage actualPerformRotateResult = rotate.performRotate(new BufferedImage(4, 1, 1));

    // Assert
    assertEquals(2, actualPerformRotateResult.getHeight());
    assertEquals(2, actualPerformRotateResult.getTileHeight());
    assertEquals(2, actualPerformRotateResult.getTileWidth());
    assertEquals(2, actualPerformRotateResult.getWidth());
  }

  /**
   * Test {@link Rotate#executeTransformOperation(BufferedImage)}.
   * <ul>
   *   <li>Given {@link Draw} (default constructor) addRotate {@link Rotate} (default constructor).</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rotate#executeTransformOperation(BufferedImage)}
   */
  @Test
  public void testExecuteTransformOperation_givenDrawAddRotateRotate_thenReturnNull() {
    // Arrange
    Draw instr = new Draw();
    instr.addRotate(new Rotate());

    Rotate rotate = new Rotate();
    rotate.addDraw(instr);
    rotate.addRotate(new Rotate());

    // Act and Assert
    assertNull(rotate.executeTransformOperation(new BufferedImage(1, 1, 1)));
  }

  /**
   * Test {@link Rotate#executeTransformOperation(BufferedImage)}.
   * <ul>
   *   <li>Given {@link Rotate} (default constructor) addDraw {@link Draw} (default constructor).</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rotate#executeTransformOperation(BufferedImage)}
   */
  @Test
  public void testExecuteTransformOperation_givenRotateAddDrawDraw_thenReturnNull() {
    // Arrange
    Rotate rotate = new Rotate();
    rotate.addDraw(new Draw());
    rotate.addRotate(new Rotate());

    // Act and Assert
    assertNull(rotate.executeTransformOperation(new BufferedImage(1, 1, 1)));
  }

  /**
   * Test {@link Rotate#executeTransformOperation(BufferedImage)}.
   * <ul>
   *   <li>Given {@link Rotate} (default constructor) addDraw {@link Draw} (default constructor).</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rotate#executeTransformOperation(BufferedImage)}
   */
  @Test
  public void testExecuteTransformOperation_givenRotateAddDrawDraw_thenReturnNull2() {
    // Arrange
    Rotate instr = new Rotate();
    instr.addDraw(new Draw());
    instr.addRotate(new Rotate());

    Rotate rotate = new Rotate();
    rotate.addRotate(instr);

    // Act and Assert
    assertNull(rotate.executeTransformOperation(new BufferedImage(1, 1, 1)));
  }

  /**
   * Test {@link Rotate#executeTransformOperation(BufferedImage)}.
   * <ul>
   *   <li>Given {@link Rotate} (default constructor) addRotate {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rotate#executeTransformOperation(BufferedImage)}
   */
  @Test
  public void testExecuteTransformOperation_givenRotateAddRotateNull() {
    // Arrange
    Rotate rotate = new Rotate();
    rotate.addRotate(null);

    // Act
    BufferedImage actualExecuteTransformOperationResult = rotate.executeTransformOperation(new BufferedImage(1, 1, 1));

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
   * Test {@link Rotate#executeTransformOperation(BufferedImage)}.
   * <ul>
   *   <li>Given {@link Rotate} (default constructor) addRotate {@link Rotate} (default constructor).</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rotate#executeTransformOperation(BufferedImage)}
   */
  @Test
  public void testExecuteTransformOperation_givenRotateAddRotateRotate_thenReturnNull() {
    // Arrange
    Rotate rotate = new Rotate();
    rotate.addRotate(new Rotate());

    // Act and Assert
    assertNull(rotate.executeTransformOperation(new BufferedImage(1, 1, 1)));
  }

  /**
   * Test {@link Rotate#executeTransformOperation(BufferedImage)}.
   * <ul>
   *   <li>Given {@link Rotate} (default constructor) addRotate {@link Rotate} (default constructor).</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rotate#executeTransformOperation(BufferedImage)}
   */
  @Test
  public void testExecuteTransformOperation_givenRotateAddRotateRotate_thenReturnNull2() {
    // Arrange
    Rotate instr = new Rotate();
    instr.addRotate(new Rotate());

    Rotate rotate = new Rotate();
    rotate.addRotate(instr);

    // Act and Assert
    assertNull(rotate.executeTransformOperation(new BufferedImage(1, 1, 1)));
  }

  /**
   * Test {@link Rotate#executeTransformOperation(BufferedImage)}.
   * <ul>
   *   <li>Given {@link Rotate} (default constructor) addScale {@link Scale} (default constructor).</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rotate#executeTransformOperation(BufferedImage)}
   */
  @Test
  public void testExecuteTransformOperation_givenRotateAddScaleScale_thenReturnNull() {
    // Arrange
    Rotate rotate = new Rotate();
    rotate.addScale(new Scale());
    rotate.addRotate(new Rotate());

    // Act and Assert
    assertNull(rotate.executeTransformOperation(new BufferedImage(1, 1, 1)));
  }

  /**
   * Test {@link Rotate#executeTransformOperation(BufferedImage)}.
   * <ul>
   *   <li>Given {@link Rotate} (default constructor) addScale {@link Scale} (default constructor).</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rotate#executeTransformOperation(BufferedImage)}
   */
  @Test
  public void testExecuteTransformOperation_givenRotateAddScaleScale_thenReturnNull2() {
    // Arrange
    Rotate instr = new Rotate();
    instr.addScale(new Scale());
    instr.addRotate(new Rotate());

    Rotate rotate = new Rotate();
    rotate.addRotate(instr);

    // Act and Assert
    assertNull(rotate.executeTransformOperation(new BufferedImage(1, 1, 1)));
  }

  /**
   * Test {@link Rotate#executeTransformOperation(BufferedImage)}.
   * <ul>
   *   <li>Given {@link Rotate} (default constructor).</li>
   *   <li>Then ColorModel return {@link DirectColorModel}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rotate#executeTransformOperation(BufferedImage)}
   */
  @Test
  public void testExecuteTransformOperation_givenRotate_thenColorModelReturnDirectColorModel() {
    // Arrange
    Rotate rotate = new Rotate();

    // Act
    BufferedImage actualExecuteTransformOperationResult = rotate.executeTransformOperation(new BufferedImage(1, 1, 1));

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
   * Test {@link Rotate#executeDrawOperation()}.
   * <ul>
   *   <li>Given {@link Rotate} (default constructor) addDraw {@link Draw} (default constructor).</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rotate#executeDrawOperation()}
   */
  @Test
  public void testExecuteDrawOperation_givenRotateAddDrawDraw_thenReturnNull() {
    // Arrange
    Rotate rotate = new Rotate();
    rotate.addDraw(new Draw());
    rotate.addRotate(new Rotate());

    // Act and Assert
    assertNull(rotate.executeDrawOperation());
  }

  /**
   * Test {@link Rotate#executeDrawOperation()}.
   * <ul>
   *   <li>Given {@link Rotate} (default constructor) addRotate {@link Rotate} (default constructor).</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rotate#executeDrawOperation()}
   */
  @Test
  public void testExecuteDrawOperation_givenRotateAddRotateRotate_thenReturnNull() {
    // Arrange
    Rotate rotate = new Rotate();
    rotate.addRotate(new Rotate());

    // Act and Assert
    assertNull(rotate.executeDrawOperation());
  }

  /**
   * Test {@link Rotate#executeDrawOperation()}.
   * <ul>
   *   <li>Given {@link Rotate} (default constructor) addScale {@link Scale} (default constructor).</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rotate#executeDrawOperation()}
   */
  @Test
  public void testExecuteDrawOperation_givenRotateAddScaleScale_thenReturnNull() {
    // Arrange
    Rotate rotate = new Rotate();
    rotate.addScale(new Scale());
    rotate.addRotate(new Rotate());

    // Act and Assert
    assertNull(rotate.executeDrawOperation());
  }

  /**
   * Test {@link Rotate#executeDrawOperation()}.
   * <ul>
   *   <li>Given {@link Rotate} (default constructor).</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rotate#executeDrawOperation()}
   */
  @Test
  public void testExecuteDrawOperation_givenRotate_thenReturnNull() {
    // Arrange, Act and Assert
    assertNull((new Rotate()).executeDrawOperation());
  }

  /**
   * Test new {@link Rotate} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Rotate}
   */
  @Test
  public void testNewRotate() {
    // Arrange and Act
    Rotate actualRotate = new Rotate();

    // Assert
    Location location = actualRotate.getLocation();
    assertNull(location.getFileName());
    assertNull(actualRotate.getDescription());
    assertNull(actualRotate.getProject());
    assertNull(actualRotate.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(actualRotate.isReference());
    assertTrue(actualRotate.instructions.isEmpty());
  }
}
