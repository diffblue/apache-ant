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

public class EllipseDiffblueTest {
  /**
   * Test {@link Ellipse#executeDrawOperation()}.
   * <ul>
   *   <li>Given {@link Ellipse} (default constructor) addDraw {@link Draw} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Ellipse#executeDrawOperation()}
   */
  @Test
  public void testExecuteDrawOperation_givenEllipseAddDrawDraw() {
    // Arrange
    Ellipse ellipse = new Ellipse();
    ellipse.addDraw(new Draw());
    ellipse.addRotate(new Rotate());
    ellipse.setHeight(1);
    ellipse.setWidth(7);

    // Act
    BufferedImage actualExecuteDrawOperationResult = ellipse.executeDrawOperation();

    // Assert
    SampleModel sampleModel = actualExecuteDrawOperationResult.getSampleModel();
    assertTrue(sampleModel instanceof PixelInterleavedSampleModel);
    assertArrayEquals(new int[]{0, 0, 0, 0}, ((PixelInterleavedSampleModel) sampleModel).getBankIndices());
    assertArrayEquals(new int[]{3, 2, 1, 0}, ((PixelInterleavedSampleModel) sampleModel).getBandOffsets());
    assertArrayEquals(new int[]{8, 8, 8, 8}, actualExecuteDrawOperationResult.getColorModel().getComponentSize());
    assertArrayEquals(new int[]{8, 8, 8, 8}, sampleModel.getSampleSize());
  }

  /**
   * Test {@link Ellipse#executeDrawOperation()}.
   * <ul>
   *   <li>Given {@link Ellipse} (default constructor) addRotate {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Ellipse#executeDrawOperation()}
   */
  @Test
  public void testExecuteDrawOperation_givenEllipseAddRotateNull() {
    // Arrange
    Ellipse ellipse = new Ellipse();
    ellipse.addRotate(null);
    ellipse.setHeight(1);
    ellipse.setWidth(7);

    // Act
    BufferedImage actualExecuteDrawOperationResult = ellipse.executeDrawOperation();

    // Assert
    SampleModel sampleModel = actualExecuteDrawOperationResult.getSampleModel();
    assertTrue(sampleModel instanceof PixelInterleavedSampleModel);
    assertArrayEquals(new int[]{0, 0, 0, 0}, ((PixelInterleavedSampleModel) sampleModel).getBankIndices());
    assertArrayEquals(new int[]{3, 2, 1, 0}, ((PixelInterleavedSampleModel) sampleModel).getBandOffsets());
    assertArrayEquals(new int[]{8, 8, 8, 8}, actualExecuteDrawOperationResult.getColorModel().getComponentSize());
    assertArrayEquals(new int[]{8, 8, 8, 8}, sampleModel.getSampleSize());
  }

  /**
   * Test {@link Ellipse#executeDrawOperation()}.
   * <ul>
   *   <li>Given {@link Ellipse} (default constructor) Stroke is {@code Col}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Ellipse#executeDrawOperation()}
   */
  @Test
  public void testExecuteDrawOperation_givenEllipseStrokeIsCol() {
    // Arrange
    Ellipse ellipse = new Ellipse();
    ellipse.setStroke("Col");
    ellipse.addRotate(new Rotate());
    ellipse.setHeight(1);
    ellipse.setWidth(7);

    // Act
    BufferedImage actualExecuteDrawOperationResult = ellipse.executeDrawOperation();

    // Assert
    SampleModel sampleModel = actualExecuteDrawOperationResult.getSampleModel();
    assertTrue(sampleModel instanceof PixelInterleavedSampleModel);
    assertArrayEquals(new int[]{0, 0, 0, 0}, ((PixelInterleavedSampleModel) sampleModel).getBankIndices());
    assertArrayEquals(new int[]{3, 2, 1, 0}, ((PixelInterleavedSampleModel) sampleModel).getBandOffsets());
    assertArrayEquals(new int[]{8, 8, 8, 8}, actualExecuteDrawOperationResult.getColorModel().getComponentSize());
    assertArrayEquals(new int[]{8, 8, 8, 8}, sampleModel.getSampleSize());
  }

  /**
   * Test {@link Ellipse#executeDrawOperation()}.
   * <ul>
   *   <li>Given {@link Ellipse} (default constructor) Stroke is {@code transparent}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Ellipse#executeDrawOperation()}
   */
  @Test
  public void testExecuteDrawOperation_givenEllipseStrokeIsTransparent() {
    // Arrange
    Ellipse ellipse = new Ellipse();
    ellipse.setStroke("transparent");
    ellipse.addRotate(new Rotate());
    ellipse.setHeight(1);
    ellipse.setWidth(7);

    // Act
    BufferedImage actualExecuteDrawOperationResult = ellipse.executeDrawOperation();

    // Assert
    SampleModel sampleModel = actualExecuteDrawOperationResult.getSampleModel();
    assertTrue(sampleModel instanceof PixelInterleavedSampleModel);
    assertArrayEquals(new int[]{0, 0, 0, 0}, ((PixelInterleavedSampleModel) sampleModel).getBankIndices());
    assertArrayEquals(new int[]{3, 2, 1, 0}, ((PixelInterleavedSampleModel) sampleModel).getBandOffsets());
    assertArrayEquals(new int[]{8, 8, 8, 8}, actualExecuteDrawOperationResult.getColorModel().getComponentSize());
    assertArrayEquals(new int[]{8, 8, 8, 8}, sampleModel.getSampleSize());
  }

  /**
   * Test {@link Ellipse#executeDrawOperation()}.
   * <ul>
   *   <li>Given {@link Text} (default constructor) String is {@code transparent}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Ellipse#executeDrawOperation()}
   */
  @Test
  public void testExecuteDrawOperation_givenTextStringIsTransparent() {
    // Arrange
    Text text = new Text();
    text.setString("transparent");

    Draw instr = new Draw();
    instr.addText(text);

    Ellipse ellipse = new Ellipse();
    ellipse.addDraw(instr);
    ellipse.addRotate(new Rotate());
    ellipse.setHeight(1);
    ellipse.setWidth(7);

    // Act
    BufferedImage actualExecuteDrawOperationResult = ellipse.executeDrawOperation();

    // Assert
    SampleModel sampleModel = actualExecuteDrawOperationResult.getSampleModel();
    assertTrue(sampleModel instanceof PixelInterleavedSampleModel);
    assertArrayEquals(new int[]{0, 0, 0, 0}, ((PixelInterleavedSampleModel) sampleModel).getBankIndices());
    assertArrayEquals(new int[]{3, 2, 1, 0}, ((PixelInterleavedSampleModel) sampleModel).getBandOffsets());
    assertArrayEquals(new int[]{8, 8, 8, 8}, actualExecuteDrawOperationResult.getColorModel().getComponentSize());
    assertArrayEquals(new int[]{8, 8, 8, 8}, sampleModel.getSampleSize());
  }

  /**
   * Test {@link Ellipse#executeDrawOperation()}.
   * <ul>
   *   <li>Then SampleModel return {@link PixelInterleavedSampleModel}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Ellipse#executeDrawOperation()}
   */
  @Test
  public void testExecuteDrawOperation_thenSampleModelReturnPixelInterleavedSampleModel() {
    // Arrange
    Ellipse ellipse = new Ellipse();
    ellipse.setHeight(1);
    ellipse.setWidth(7);

    // Act
    BufferedImage actualExecuteDrawOperationResult = ellipse.executeDrawOperation();

    // Assert
    SampleModel sampleModel = actualExecuteDrawOperationResult.getSampleModel();
    assertTrue(sampleModel instanceof PixelInterleavedSampleModel);
    assertArrayEquals(new int[]{0, 0, 0, 0}, ((PixelInterleavedSampleModel) sampleModel).getBankIndices());
    assertArrayEquals(new int[]{3, 2, 1, 0}, ((PixelInterleavedSampleModel) sampleModel).getBandOffsets());
    assertArrayEquals(new int[]{8, 8, 8, 8}, actualExecuteDrawOperationResult.getColorModel().getComponentSize());
    assertArrayEquals(new int[]{8, 8, 8, 8}, sampleModel.getSampleSize());
  }

  /**
   * Test {@link Ellipse#executeDrawOperation()}.
   * <ul>
   *   <li>Then SampleModel return {@link PixelInterleavedSampleModel}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Ellipse#executeDrawOperation()}
   */
  @Test
  public void testExecuteDrawOperation_thenSampleModelReturnPixelInterleavedSampleModel2() {
    // Arrange
    Ellipse ellipse = new Ellipse();
    ellipse.addRotate(new Rotate());
    ellipse.setHeight(1);
    ellipse.setWidth(7);

    // Act
    BufferedImage actualExecuteDrawOperationResult = ellipse.executeDrawOperation();

    // Assert
    SampleModel sampleModel = actualExecuteDrawOperationResult.getSampleModel();
    assertTrue(sampleModel instanceof PixelInterleavedSampleModel);
    assertArrayEquals(new int[]{0, 0, 0, 0}, ((PixelInterleavedSampleModel) sampleModel).getBankIndices());
    assertArrayEquals(new int[]{3, 2, 1, 0}, ((PixelInterleavedSampleModel) sampleModel).getBandOffsets());
    assertArrayEquals(new int[]{8, 8, 8, 8}, actualExecuteDrawOperationResult.getColorModel().getComponentSize());
    assertArrayEquals(new int[]{8, 8, 8, 8}, sampleModel.getSampleSize());
  }

  /**
   * Test new {@link Ellipse} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Ellipse}
   */
  @Test
  public void testNewEllipse() {
    // Arrange and Act
    Ellipse actualEllipse = new Ellipse();

    // Assert
    assertEquals("transparent", actualEllipse.fill);
    Location location = actualEllipse.getLocation();
    assertNull(location.getFileName());
    assertNull(actualEllipse.getDescription());
    assertNull(actualEllipse.getProject());
    assertNull(actualEllipse.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualEllipse.height);
    assertEquals(0, actualEllipse.strokeWidth);
    assertEquals(0, actualEllipse.width);
    assertFalse(actualEllipse.isReference());
    assertTrue(actualEllipse.instructions.isEmpty());
    assertEquals(ColorMapper.COLOR_BLACK, actualEllipse.stroke);
  }
}
