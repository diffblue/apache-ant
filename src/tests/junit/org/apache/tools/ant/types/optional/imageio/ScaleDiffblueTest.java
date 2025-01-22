package org.apache.tools.ant.types.optional.imageio;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import java.awt.image.BufferedImage;
import java.awt.image.ColorModel;
import java.awt.image.DirectColorModel;
import java.awt.image.SampleModel;
import java.awt.image.SinglePixelPackedSampleModel;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.types.optional.imageio.Scale.ProportionsAttribute;
import org.junit.Test;

public class ScaleDiffblueTest {
  /**
   * Test {@link Scale#getWidth()}.
   * <ul>
   *   <li>Given {@link Scale} (default constructor) Width is {@code 42}.</li>
   *   <li>Then return forty-two.</li>
   * </ul>
   * <p>
   * Method under test: {@link Scale#getWidth()}
   */
  @Test
  public void testGetWidth_givenScaleWidthIs42_thenReturnFortyTwo() {
    // Arrange
    Scale scale = new Scale();
    scale.setWidth("42");

    // Act and Assert
    assertEquals(42.0f, scale.getWidth(), 0.0f);
  }

  /**
   * Test {@link Scale#getWidth()}.
   * <ul>
   *   <li>Given {@link Scale} (default constructor).</li>
   *   <li>Then return one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Scale#getWidth()}
   */
  @Test
  public void testGetWidth_givenScale_thenReturnOne() {
    // Arrange, Act and Assert
    assertEquals(1.0f, (new Scale()).getWidth(), 0.0f);
  }

  /**
   * Test {@link Scale#getHeight()}.
   * <ul>
   *   <li>Given {@link Scale} (default constructor) Height is {@code 42}.</li>
   *   <li>Then return forty-two.</li>
   * </ul>
   * <p>
   * Method under test: {@link Scale#getHeight()}
   */
  @Test
  public void testGetHeight_givenScaleHeightIs42_thenReturnFortyTwo() {
    // Arrange
    Scale scale = new Scale();
    scale.setHeight("42");

    // Act and Assert
    assertEquals(42.0f, scale.getHeight(), 0.0f);
  }

  /**
   * Test {@link Scale#getHeight()}.
   * <ul>
   *   <li>Given {@link Scale} (default constructor).</li>
   *   <li>Then return one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Scale#getHeight()}
   */
  @Test
  public void testGetHeight_givenScale_thenReturnOne() {
    // Arrange, Act and Assert
    assertEquals(1.0f, (new Scale()).getHeight(), 0.0f);
  }

  /**
   * Test {@link Scale#performScale(BufferedImage)}.
   * <ul>
   *   <li>Given {@link Scale} (default constructor) Height is {@code 42}.</li>
   *   <li>Then return Height is forty-two.</li>
   * </ul>
   * <p>
   * Method under test: {@link Scale#performScale(BufferedImage)}
   */
  @Test
  public void testPerformScale_givenScaleHeightIs42_thenReturnHeightIsFortyTwo() {
    // Arrange
    Scale scale = new Scale();
    scale.setHeight("42");

    // Act
    BufferedImage actualPerformScaleResult = scale.performScale(new BufferedImage(1, 1, 1));

    // Assert
    ColorModel colorModel = actualPerformScaleResult.getColorModel();
    assertTrue(colorModel instanceof DirectColorModel);
    SampleModel sampleModel = actualPerformScaleResult.getSampleModel();
    assertTrue(sampleModel instanceof SinglePixelPackedSampleModel);
    assertEquals(42, actualPerformScaleResult.getHeight());
    assertEquals(42, actualPerformScaleResult.getTileHeight());
    assertEquals(42, actualPerformScaleResult.getData().getHeight());
    assertEquals(42, actualPerformScaleResult.getRaster().getHeight());
    assertEquals(42, sampleModel.getHeight());
    assertArrayEquals(new int[]{16711680, 65280, 255}, ((DirectColorModel) colorModel).getMasks());
    assertArrayEquals(new int[]{16711680, 65280, 255}, ((SinglePixelPackedSampleModel) sampleModel).getBitMasks());
    assertArrayEquals(new int[]{8, 8, 8}, colorModel.getComponentSize());
    assertArrayEquals(new int[]{8, 8, 8}, sampleModel.getSampleSize());
    assertArrayEquals(new int[]{Short.SIZE, 8, 0}, ((SinglePixelPackedSampleModel) sampleModel).getBitOffsets());
  }

  /**
   * Test {@link Scale#performScale(BufferedImage)}.
   * <ul>
   *   <li>Given {@link Scale} (default constructor) Width is {@code 42}.</li>
   *   <li>Then return TileWidth is forty-two.</li>
   * </ul>
   * <p>
   * Method under test: {@link Scale#performScale(BufferedImage)}
   */
  @Test
  public void testPerformScale_givenScaleWidthIs42_thenReturnTileWidthIsFortyTwo() {
    // Arrange
    Scale scale = new Scale();
    scale.setWidth("42");

    // Act
    BufferedImage actualPerformScaleResult = scale.performScale(new BufferedImage(1, 1, 1));

    // Assert
    ColorModel colorModel = actualPerformScaleResult.getColorModel();
    assertTrue(colorModel instanceof DirectColorModel);
    SampleModel sampleModel = actualPerformScaleResult.getSampleModel();
    assertTrue(sampleModel instanceof SinglePixelPackedSampleModel);
    assertEquals(42, actualPerformScaleResult.getTileWidth());
    assertEquals(42, actualPerformScaleResult.getWidth());
    assertEquals(42, actualPerformScaleResult.getData().getWidth());
    assertEquals(42, actualPerformScaleResult.getRaster().getWidth());
    assertEquals(42, sampleModel.getWidth());
    assertEquals(42, ((SinglePixelPackedSampleModel) sampleModel).getScanlineStride());
    assertArrayEquals(new int[]{16711680, 65280, 255}, ((DirectColorModel) colorModel).getMasks());
    assertArrayEquals(new int[]{16711680, 65280, 255}, ((SinglePixelPackedSampleModel) sampleModel).getBitMasks());
    assertArrayEquals(new int[]{8, 8, 8}, colorModel.getComponentSize());
    assertArrayEquals(new int[]{8, 8, 8}, sampleModel.getSampleSize());
    assertArrayEquals(new int[]{Short.SIZE, 8, 0}, ((SinglePixelPackedSampleModel) sampleModel).getBitOffsets());
  }

  /**
   * Test {@link Scale#performScale(BufferedImage)}.
   * <ul>
   *   <li>Given {@link Scale} (default constructor).</li>
   *   <li>Then ColorModel return {@link DirectColorModel}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Scale#performScale(BufferedImage)}
   */
  @Test
  public void testPerformScale_givenScale_thenColorModelReturnDirectColorModel() {
    // Arrange
    Scale scale = new Scale();

    // Act
    BufferedImage actualPerformScaleResult = scale.performScale(new BufferedImage(1, 1, 1));

    // Assert
    ColorModel colorModel = actualPerformScaleResult.getColorModel();
    assertTrue(colorModel instanceof DirectColorModel);
    SampleModel sampleModel = actualPerformScaleResult.getSampleModel();
    assertTrue(sampleModel instanceof SinglePixelPackedSampleModel);
    assertArrayEquals(new int[]{16711680, 65280, 255}, ((DirectColorModel) colorModel).getMasks());
    assertArrayEquals(new int[]{16711680, 65280, 255}, ((SinglePixelPackedSampleModel) sampleModel).getBitMasks());
    assertArrayEquals(new int[]{8, 8, 8}, colorModel.getComponentSize());
    assertArrayEquals(new int[]{8, 8, 8}, sampleModel.getSampleSize());
    assertArrayEquals(new int[]{Short.SIZE, 8, 0}, ((SinglePixelPackedSampleModel) sampleModel).getBitOffsets());
  }

  /**
   * Test {@link Scale#executeTransformOperation(BufferedImage)}.
   * <ul>
   *   <li>Given {@link Draw} (default constructor) addScale {@link Scale} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Scale#executeTransformOperation(BufferedImage)}
   */
  @Test
  public void testExecuteTransformOperation_givenDrawAddScaleScale() {
    // Arrange
    Draw instr = new Draw();
    instr.addScale(new Scale());

    Scale scale = new Scale();
    scale.addDraw(instr);
    scale.addRotate(new Rotate());

    // Act
    BufferedImage actualExecuteTransformOperationResult = scale.executeTransformOperation(new BufferedImage(1, 1, 1));

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
   * Test {@link Scale#executeTransformOperation(BufferedImage)}.
   * <ul>
   *   <li>Given {@link Draw} (default constructor) addScale {@link Scale} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Scale#executeTransformOperation(BufferedImage)}
   */
  @Test
  public void testExecuteTransformOperation_givenDrawAddScaleScale2() {
    // Arrange
    Draw instr = new Draw();
    instr.addScale(new Scale());
    instr.addScale(new Scale());

    Scale scale = new Scale();
    scale.addDraw(instr);
    scale.addRotate(new Rotate());

    // Act
    BufferedImage actualExecuteTransformOperationResult = scale.executeTransformOperation(new BufferedImage(1, 1, 1));

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
   * Test {@link Scale#executeTransformOperation(BufferedImage)}.
   * <ul>
   *   <li>Given {@link Scale} (default constructor) addDraw {@link Draw} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Scale#executeTransformOperation(BufferedImage)}
   */
  @Test
  public void testExecuteTransformOperation_givenScaleAddDrawDraw() {
    // Arrange
    Scale scale = new Scale();
    scale.addDraw(new Draw());
    scale.addRotate(new Rotate());

    // Act
    BufferedImage actualExecuteTransformOperationResult = scale.executeTransformOperation(new BufferedImage(1, 1, 1));

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
   * Test {@link Scale#executeTransformOperation(BufferedImage)}.
   * <ul>
   *   <li>Given {@link Scale} (default constructor) addRotate {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Scale#executeTransformOperation(BufferedImage)}
   */
  @Test
  public void testExecuteTransformOperation_givenScaleAddRotateNull() {
    // Arrange
    Scale scale = new Scale();
    scale.addRotate(null);

    // Act
    BufferedImage actualExecuteTransformOperationResult = scale.executeTransformOperation(new BufferedImage(1, 1, 1));

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
   * Test {@link Scale#executeTransformOperation(BufferedImage)}.
   * <ul>
   *   <li>Given {@link Scale} (default constructor) Height is {@code 42}.</li>
   *   <li>Then return Height is forty-two.</li>
   * </ul>
   * <p>
   * Method under test: {@link Scale#executeTransformOperation(BufferedImage)}
   */
  @Test
  public void testExecuteTransformOperation_givenScaleHeightIs42_thenReturnHeightIsFortyTwo() {
    // Arrange
    Scale scale = new Scale();
    scale.setHeight("42");
    scale.addRotate(new Rotate());

    // Act
    BufferedImage actualExecuteTransformOperationResult = scale.executeTransformOperation(new BufferedImage(1, 1, 1));

    // Assert
    ColorModel colorModel = actualExecuteTransformOperationResult.getColorModel();
    assertTrue(colorModel instanceof DirectColorModel);
    SampleModel sampleModel = actualExecuteTransformOperationResult.getSampleModel();
    assertTrue(sampleModel instanceof SinglePixelPackedSampleModel);
    assertEquals(42, actualExecuteTransformOperationResult.getHeight());
    assertEquals(42, actualExecuteTransformOperationResult.getTileHeight());
    assertEquals(42, actualExecuteTransformOperationResult.getData().getHeight());
    assertEquals(42, actualExecuteTransformOperationResult.getRaster().getHeight());
    assertEquals(42, sampleModel.getHeight());
    assertArrayEquals(new int[]{16711680, 65280, 255}, ((DirectColorModel) colorModel).getMasks());
    assertArrayEquals(new int[]{16711680, 65280, 255}, ((SinglePixelPackedSampleModel) sampleModel).getBitMasks());
    assertArrayEquals(new int[]{8, 8, 8}, colorModel.getComponentSize());
    assertArrayEquals(new int[]{8, 8, 8}, sampleModel.getSampleSize());
    assertArrayEquals(new int[]{Short.SIZE, 8, 0}, ((SinglePixelPackedSampleModel) sampleModel).getBitOffsets());
  }

  /**
   * Test {@link Scale#executeTransformOperation(BufferedImage)}.
   * <ul>
   *   <li>Given {@link Scale} (default constructor) Width is {@code 42}.</li>
   *   <li>Then return TileWidth is forty-two.</li>
   * </ul>
   * <p>
   * Method under test: {@link Scale#executeTransformOperation(BufferedImage)}
   */
  @Test
  public void testExecuteTransformOperation_givenScaleWidthIs42_thenReturnTileWidthIsFortyTwo() {
    // Arrange
    Scale scale = new Scale();
    scale.setWidth("42");
    scale.addRotate(new Rotate());

    // Act
    BufferedImage actualExecuteTransformOperationResult = scale.executeTransformOperation(new BufferedImage(1, 1, 1));

    // Assert
    ColorModel colorModel = actualExecuteTransformOperationResult.getColorModel();
    assertTrue(colorModel instanceof DirectColorModel);
    SampleModel sampleModel = actualExecuteTransformOperationResult.getSampleModel();
    assertTrue(sampleModel instanceof SinglePixelPackedSampleModel);
    assertEquals(42, actualExecuteTransformOperationResult.getTileWidth());
    assertEquals(42, actualExecuteTransformOperationResult.getWidth());
    assertEquals(42, actualExecuteTransformOperationResult.getData().getWidth());
    assertEquals(42, actualExecuteTransformOperationResult.getRaster().getWidth());
    assertEquals(42, sampleModel.getWidth());
    assertEquals(42, ((SinglePixelPackedSampleModel) sampleModel).getScanlineStride());
    assertArrayEquals(new int[]{16711680, 65280, 255}, ((DirectColorModel) colorModel).getMasks());
    assertArrayEquals(new int[]{16711680, 65280, 255}, ((SinglePixelPackedSampleModel) sampleModel).getBitMasks());
    assertArrayEquals(new int[]{8, 8, 8}, colorModel.getComponentSize());
    assertArrayEquals(new int[]{8, 8, 8}, sampleModel.getSampleSize());
    assertArrayEquals(new int[]{Short.SIZE, 8, 0}, ((SinglePixelPackedSampleModel) sampleModel).getBitOffsets());
  }

  /**
   * Test {@link Scale#executeTransformOperation(BufferedImage)}.
   * <ul>
   *   <li>Given {@link Scale} (default constructor).</li>
   *   <li>Then ColorModel return {@link DirectColorModel}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Scale#executeTransformOperation(BufferedImage)}
   */
  @Test
  public void testExecuteTransformOperation_givenScale_thenColorModelReturnDirectColorModel() {
    // Arrange
    Scale scale = new Scale();

    // Act
    BufferedImage actualExecuteTransformOperationResult = scale.executeTransformOperation(new BufferedImage(1, 1, 1));

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
   * Test {@link Scale#executeTransformOperation(BufferedImage)}.
   * <ul>
   *   <li>Then ColorModel return {@link DirectColorModel}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Scale#executeTransformOperation(BufferedImage)}
   */
  @Test
  public void testExecuteTransformOperation_thenColorModelReturnDirectColorModel() {
    // Arrange
    Scale scale = new Scale();
    scale.addRotate(new Rotate());

    // Act
    BufferedImage actualExecuteTransformOperationResult = scale.executeTransformOperation(new BufferedImage(1, 1, 1));

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
   * Test {@link Scale#executeDrawOperation()}.
   * <ul>
   *   <li>Given {@link Scale} (default constructor).</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Scale#executeDrawOperation()}
   */
  @Test
  public void testExecuteDrawOperation_givenScale_thenReturnNull() {
    // Arrange, Act and Assert
    assertNull((new Scale()).executeDrawOperation());
  }

  /**
   * Test new {@link Scale} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Scale}
   */
  @Test
  public void testNewScale() {
    // Arrange and Act
    Scale actualScale = new Scale();

    // Assert
    Location location = actualScale.getLocation();
    assertNull(location.getFileName());
    assertNull(actualScale.getDescription());
    assertNull(actualScale.getProject());
    assertNull(actualScale.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(1.0f, actualScale.getHeight(), 0.0f);
    assertEquals(1.0f, actualScale.getWidth(), 0.0f);
    assertFalse(actualScale.isReference());
    assertTrue(actualScale.instructions.isEmpty());
  }

  /**
   * Test ProportionsAttribute {@link ProportionsAttribute#getValues()}.
   * <p>
   * Method under test: {@link ProportionsAttribute#getValues()}
   */
  @Test
  public void testProportionsAttributeGetValues() {
    // Arrange, Act and Assert
    assertArrayEquals(new String[]{"ignore", "width", "height", "cover", "fit"},
        (new ProportionsAttribute()).getValues());
  }

  /**
   * Test ProportionsAttribute new {@link ProportionsAttribute} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link ProportionsAttribute}
   */
  @Test
  public void testProportionsAttributeNewProportionsAttribute() {
    // Arrange and Act
    ProportionsAttribute actualProportionsAttribute = new ProportionsAttribute();

    // Assert
    assertNull(actualProportionsAttribute.getValue());
    assertEquals(-1, actualProportionsAttribute.getIndex());
  }
}
