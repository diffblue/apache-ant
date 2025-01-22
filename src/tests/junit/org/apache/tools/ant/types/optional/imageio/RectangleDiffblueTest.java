package org.apache.tools.ant.types.optional.imageio;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import java.awt.image.BufferedImage;
import java.awt.image.PixelInterleavedSampleModel;
import java.awt.image.SampleModel;
import org.apache.tools.ant.Location;
import org.junit.Test;

public class RectangleDiffblueTest {
  /**
   * Test {@link Rectangle#executeDrawOperation()}.
   * <ul>
   *   <li>Given {@link Rectangle} (default constructor) addDraw {@link Draw} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Rectangle#executeDrawOperation()}
   */
  @Test
  public void testExecuteDrawOperation_givenRectangleAddDrawDraw() {
    // Arrange
    Rectangle rectangle = new Rectangle();
    rectangle.addDraw(new Draw());
    rectangle.addRotate(new Rotate());
    rectangle.setHeight(1);
    rectangle.setWidth(2);

    // Act
    BufferedImage actualExecuteDrawOperationResult = rectangle.executeDrawOperation();

    // Assert
    SampleModel sampleModel = actualExecuteDrawOperationResult.getSampleModel();
    assertTrue(sampleModel instanceof PixelInterleavedSampleModel);
    assertArrayEquals(new int[]{0, 0, 0, 0}, ((PixelInterleavedSampleModel) sampleModel).getBankIndices());
    assertArrayEquals(new int[]{3, 2, 1, 0}, ((PixelInterleavedSampleModel) sampleModel).getBandOffsets());
    assertArrayEquals(new int[]{8, 8, 8, 8}, actualExecuteDrawOperationResult.getColorModel().getComponentSize());
    assertArrayEquals(new int[]{8, 8, 8, 8}, sampleModel.getSampleSize());
  }

  /**
   * Test {@link Rectangle#executeDrawOperation()}.
   * <ul>
   *   <li>Given {@link Rectangle} (default constructor) addRotate {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rectangle#executeDrawOperation()}
   */
  @Test
  public void testExecuteDrawOperation_givenRectangleAddRotateNull() {
    // Arrange
    Rectangle rectangle = new Rectangle();
    rectangle.addRotate(null);
    rectangle.setHeight(1);
    rectangle.setWidth(2);

    // Act
    BufferedImage actualExecuteDrawOperationResult = rectangle.executeDrawOperation();

    // Assert
    SampleModel sampleModel = actualExecuteDrawOperationResult.getSampleModel();
    assertTrue(sampleModel instanceof PixelInterleavedSampleModel);
    assertArrayEquals(new int[]{0, 0, 0, 0}, ((PixelInterleavedSampleModel) sampleModel).getBankIndices());
    assertArrayEquals(new int[]{3, 2, 1, 0}, ((PixelInterleavedSampleModel) sampleModel).getBandOffsets());
    assertArrayEquals(new int[]{8, 8, 8, 8}, actualExecuteDrawOperationResult.getColorModel().getComponentSize());
    assertArrayEquals(new int[]{8, 8, 8, 8}, sampleModel.getSampleSize());
  }

  /**
   * Test {@link Rectangle#executeDrawOperation()}.
   * <ul>
   *   <li>Given {@link Rectangle} (default constructor) Archeight is two.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rectangle#executeDrawOperation()}
   */
  @Test
  public void testExecuteDrawOperation_givenRectangleArcheightIsTwo() {
    // Arrange
    Rectangle rectangle = new Rectangle();
    rectangle.setArcheight(2);
    rectangle.addRotate(new Rotate());
    rectangle.setHeight(1);
    rectangle.setWidth(2);

    // Act
    BufferedImage actualExecuteDrawOperationResult = rectangle.executeDrawOperation();

    // Assert
    SampleModel sampleModel = actualExecuteDrawOperationResult.getSampleModel();
    assertTrue(sampleModel instanceof PixelInterleavedSampleModel);
    assertArrayEquals(new int[]{0, 0, 0, 0}, ((PixelInterleavedSampleModel) sampleModel).getBankIndices());
    assertArrayEquals(new int[]{3, 2, 1, 0}, ((PixelInterleavedSampleModel) sampleModel).getBandOffsets());
    assertArrayEquals(new int[]{8, 8, 8, 8}, actualExecuteDrawOperationResult.getColorModel().getComponentSize());
    assertArrayEquals(new int[]{8, 8, 8, 8}, sampleModel.getSampleSize());
  }

  /**
   * Test {@link Rectangle#executeDrawOperation()}.
   * <ul>
   *   <li>Given {@link Rectangle} (default constructor) Arcwidth is two.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rectangle#executeDrawOperation()}
   */
  @Test
  public void testExecuteDrawOperation_givenRectangleArcwidthIsTwo() {
    // Arrange
    Rectangle rectangle = new Rectangle();
    rectangle.setArcwidth(2);
    rectangle.addRotate(new Rotate());
    rectangle.setHeight(1);
    rectangle.setWidth(2);

    // Act
    BufferedImage actualExecuteDrawOperationResult = rectangle.executeDrawOperation();

    // Assert
    SampleModel sampleModel = actualExecuteDrawOperationResult.getSampleModel();
    assertTrue(sampleModel instanceof PixelInterleavedSampleModel);
    assertArrayEquals(new int[]{0, 0, 0, 0}, ((PixelInterleavedSampleModel) sampleModel).getBankIndices());
    assertArrayEquals(new int[]{3, 2, 1, 0}, ((PixelInterleavedSampleModel) sampleModel).getBandOffsets());
    assertArrayEquals(new int[]{8, 8, 8, 8}, actualExecuteDrawOperationResult.getColorModel().getComponentSize());
    assertArrayEquals(new int[]{8, 8, 8, 8}, sampleModel.getSampleSize());
  }

  /**
   * Test {@link Rectangle#executeDrawOperation()}.
   * <ul>
   *   <li>Given {@link Rectangle} (default constructor) Fill is {@code Creating Rectangle w=}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rectangle#executeDrawOperation()}
   */
  @Test
  public void testExecuteDrawOperation_givenRectangleFillIsCreatingRectangleW() {
    // Arrange
    Rectangle rectangle = new Rectangle();
    rectangle.setFill("\tCreating Rectangle w=");
    rectangle.addRotate(new Rotate());
    rectangle.setHeight(1);
    rectangle.setWidth(2);

    // Act
    BufferedImage actualExecuteDrawOperationResult = rectangle.executeDrawOperation();

    // Assert
    SampleModel sampleModel = actualExecuteDrawOperationResult.getSampleModel();
    assertTrue(sampleModel instanceof PixelInterleavedSampleModel);
    assertArrayEquals(new int[]{0, 0, 0, 0}, ((PixelInterleavedSampleModel) sampleModel).getBankIndices());
    assertArrayEquals(new int[]{3, 2, 1, 0}, ((PixelInterleavedSampleModel) sampleModel).getBandOffsets());
    assertArrayEquals(new int[]{8, 8, 8, 8}, actualExecuteDrawOperationResult.getColorModel().getComponentSize());
    assertArrayEquals(new int[]{8, 8, 8, 8}, sampleModel.getSampleSize());
  }

  /**
   * Test {@link Rectangle#executeDrawOperation()}.
   * <ul>
   *   <li>Given {@link Rectangle} (default constructor) Fill is {@code Creating Rectangle w=}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rectangle#executeDrawOperation()}
   */
  @Test
  public void testExecuteDrawOperation_givenRectangleFillIsCreatingRectangleW2() {
    // Arrange
    Rectangle rectangle = new Rectangle();
    rectangle.setFill("\tCreating Rectangle w=");
    rectangle.setArcwidth(2);
    rectangle.addRotate(new Rotate());
    rectangle.setHeight(1);
    rectangle.setWidth(2);

    // Act
    BufferedImage actualExecuteDrawOperationResult = rectangle.executeDrawOperation();

    // Assert
    SampleModel sampleModel = actualExecuteDrawOperationResult.getSampleModel();
    assertTrue(sampleModel instanceof PixelInterleavedSampleModel);
    assertArrayEquals(new int[]{0, 0, 0, 0}, ((PixelInterleavedSampleModel) sampleModel).getBankIndices());
    assertArrayEquals(new int[]{3, 2, 1, 0}, ((PixelInterleavedSampleModel) sampleModel).getBandOffsets());
    assertArrayEquals(new int[]{8, 8, 8, 8}, actualExecuteDrawOperationResult.getColorModel().getComponentSize());
    assertArrayEquals(new int[]{8, 8, 8, 8}, sampleModel.getSampleSize());
  }

  /**
   * Test {@link Rectangle#executeDrawOperation()}.
   * <ul>
   *   <li>Given {@link Rectangle} (default constructor) Stroke is {@code Creating Rectangle w=}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rectangle#executeDrawOperation()}
   */
  @Test
  public void testExecuteDrawOperation_givenRectangleStrokeIsCreatingRectangleW() {
    // Arrange
    Rectangle rectangle = new Rectangle();
    rectangle.setStroke("\tCreating Rectangle w=");
    rectangle.addRotate(new Rotate());
    rectangle.setHeight(1);
    rectangle.setWidth(2);

    // Act
    BufferedImage actualExecuteDrawOperationResult = rectangle.executeDrawOperation();

    // Assert
    SampleModel sampleModel = actualExecuteDrawOperationResult.getSampleModel();
    assertTrue(sampleModel instanceof PixelInterleavedSampleModel);
    assertArrayEquals(new int[]{0, 0, 0, 0}, ((PixelInterleavedSampleModel) sampleModel).getBankIndices());
    assertArrayEquals(new int[]{3, 2, 1, 0}, ((PixelInterleavedSampleModel) sampleModel).getBandOffsets());
    assertArrayEquals(new int[]{8, 8, 8, 8}, actualExecuteDrawOperationResult.getColorModel().getComponentSize());
    assertArrayEquals(new int[]{8, 8, 8, 8}, sampleModel.getSampleSize());
  }

  /**
   * Test {@link Rectangle#executeDrawOperation()}.
   * <ul>
   *   <li>Given {@link Rectangle} (default constructor) Stroke is {@code transparent}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rectangle#executeDrawOperation()}
   */
  @Test
  public void testExecuteDrawOperation_givenRectangleStrokeIsTransparent() {
    // Arrange
    Rectangle rectangle = new Rectangle();
    rectangle.setStroke("transparent");
    rectangle.addRotate(new Rotate());
    rectangle.setHeight(1);
    rectangle.setWidth(2);

    // Act
    BufferedImage actualExecuteDrawOperationResult = rectangle.executeDrawOperation();

    // Assert
    SampleModel sampleModel = actualExecuteDrawOperationResult.getSampleModel();
    assertTrue(sampleModel instanceof PixelInterleavedSampleModel);
    assertArrayEquals(new int[]{0, 0, 0, 0}, ((PixelInterleavedSampleModel) sampleModel).getBankIndices());
    assertArrayEquals(new int[]{3, 2, 1, 0}, ((PixelInterleavedSampleModel) sampleModel).getBandOffsets());
    assertArrayEquals(new int[]{8, 8, 8, 8}, actualExecuteDrawOperationResult.getColorModel().getComponentSize());
    assertArrayEquals(new int[]{8, 8, 8, 8}, sampleModel.getSampleSize());
  }

  /**
   * Test {@link Rectangle#executeDrawOperation()}.
   * <ul>
   *   <li>Given {@link Text} (default constructor) String is {@code Creating Rectangle w=}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rectangle#executeDrawOperation()}
   */
  @Test
  public void testExecuteDrawOperation_givenTextStringIsCreatingRectangleW() {
    // Arrange
    Text text = new Text();
    text.setString("\tCreating Rectangle w=");

    Draw instr = new Draw();
    instr.addText(text);

    Rectangle rectangle = new Rectangle();
    rectangle.addDraw(instr);
    rectangle.addRotate(new Rotate());
    rectangle.setHeight(1);
    rectangle.setWidth(2);

    // Act
    BufferedImage actualExecuteDrawOperationResult = rectangle.executeDrawOperation();

    // Assert
    SampleModel sampleModel = actualExecuteDrawOperationResult.getSampleModel();
    assertTrue(sampleModel instanceof PixelInterleavedSampleModel);
    assertArrayEquals(new int[]{0, 0, 0, 0}, ((PixelInterleavedSampleModel) sampleModel).getBankIndices());
    assertArrayEquals(new int[]{3, 2, 1, 0}, ((PixelInterleavedSampleModel) sampleModel).getBandOffsets());
    assertArrayEquals(new int[]{8, 8, 8, 8}, actualExecuteDrawOperationResult.getColorModel().getComponentSize());
    assertArrayEquals(new int[]{8, 8, 8, 8}, sampleModel.getSampleSize());
  }

  /**
   * Test {@link Rectangle#executeDrawOperation()}.
   * <ul>
   *   <li>Then SampleModel return {@link PixelInterleavedSampleModel}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rectangle#executeDrawOperation()}
   */
  @Test
  public void testExecuteDrawOperation_thenSampleModelReturnPixelInterleavedSampleModel() {
    // Arrange
    Rectangle rectangle = new Rectangle();
    rectangle.setHeight(1);
    rectangle.setWidth(2);

    // Act
    BufferedImage actualExecuteDrawOperationResult = rectangle.executeDrawOperation();

    // Assert
    SampleModel sampleModel = actualExecuteDrawOperationResult.getSampleModel();
    assertTrue(sampleModel instanceof PixelInterleavedSampleModel);
    assertArrayEquals(new int[]{0, 0, 0, 0}, ((PixelInterleavedSampleModel) sampleModel).getBankIndices());
    assertArrayEquals(new int[]{3, 2, 1, 0}, ((PixelInterleavedSampleModel) sampleModel).getBandOffsets());
    assertArrayEquals(new int[]{8, 8, 8, 8}, actualExecuteDrawOperationResult.getColorModel().getComponentSize());
    assertArrayEquals(new int[]{8, 8, 8, 8}, sampleModel.getSampleSize());
  }

  /**
   * Test {@link Rectangle#executeDrawOperation()}.
   * <ul>
   *   <li>Then SampleModel return {@link PixelInterleavedSampleModel}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Rectangle#executeDrawOperation()}
   */
  @Test
  public void testExecuteDrawOperation_thenSampleModelReturnPixelInterleavedSampleModel2() {
    // Arrange
    Rectangle rectangle = new Rectangle();
    rectangle.addRotate(new Rotate());
    rectangle.setHeight(1);
    rectangle.setWidth(2);

    // Act
    BufferedImage actualExecuteDrawOperationResult = rectangle.executeDrawOperation();

    // Assert
    SampleModel sampleModel = actualExecuteDrawOperationResult.getSampleModel();
    assertTrue(sampleModel instanceof PixelInterleavedSampleModel);
    assertArrayEquals(new int[]{0, 0, 0, 0}, ((PixelInterleavedSampleModel) sampleModel).getBankIndices());
    assertArrayEquals(new int[]{3, 2, 1, 0}, ((PixelInterleavedSampleModel) sampleModel).getBandOffsets());
    assertArrayEquals(new int[]{8, 8, 8, 8}, actualExecuteDrawOperationResult.getColorModel().getComponentSize());
    assertArrayEquals(new int[]{8, 8, 8, 8}, sampleModel.getSampleSize());
  }

  /**
   * Test new {@link Rectangle} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Rectangle}
   */
  @Test
  public void testNewRectangle() {
    // Arrange and Act
    Rectangle actualRectangle = new Rectangle();

    // Assert
    assertEquals("transparent", actualRectangle.fill);
    Location location = actualRectangle.getLocation();
    assertNull(location.getFileName());
    assertNull(actualRectangle.getDescription());
    assertNull(actualRectangle.getProject());
    assertNull(actualRectangle.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualRectangle.height);
    assertEquals(0, actualRectangle.strokeWidth);
    assertEquals(0, actualRectangle.width);
    assertFalse(actualRectangle.isReference());
    assertTrue(actualRectangle.instructions.isEmpty());
    assertEquals(ColorMapper.COLOR_BLACK, actualRectangle.stroke);
  }
}
