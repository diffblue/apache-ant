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
import org.apache.tools.ant.types.optional.imageio.Arc.ArcType;
import org.junit.Test;

public class ArcDiffblueTest {
  /**
   * Test ArcType {@link ArcType#getDefault()}.
   * <p>
   * Method under test: {@link ArcType#getDefault()}
   */
  @Test
  public void testArcTypeGetDefault() {
    // Arrange and Act
    ArcType actualDefault = ArcType.getDefault();

    // Assert
    assertEquals("open", actualDefault.getValue());
    assertEquals(0, actualDefault.getIndex());
    assertArrayEquals(new String[]{"open", "chord", "pie"}, actualDefault.getValues());
  }

  /**
   * Test ArcType {@link ArcType#getValues()}.
   * <p>
   * Method under test: {@link ArcType#getValues()}
   */
  @Test
  public void testArcTypeGetValues() {
    // Arrange, Act and Assert
    assertArrayEquals(new String[]{"open", "chord", "pie"}, ArcType.getDefault().getValues());
  }

  /**
   * Test ArcType {@link ArcType#ArcType()}.
   * <p>
   * Method under test: {@link ArcType#ArcType()}
   */
  @Test
  public void testArcTypeNewArcType() {
    // Arrange and Act
    ArcType actualArcType = new ArcType();

    // Assert
    assertNull(actualArcType.getValue());
    assertEquals(-1, actualArcType.getIndex());
  }

  /**
   * Test ArcType {@link ArcType#ArcType(String)}.
   * <ul>
   *   <li>When {@code open}.</li>
   *   <li>Then return Value is {@code open}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArcType#ArcType(String)}
   */
  @Test
  public void testArcTypeNewArcType_whenOpen_thenReturnValueIsOpen() {
    // Arrange and Act
    ArcType actualArcType = new ArcType("open");

    // Assert
    assertEquals("open", actualArcType.getValue());
    assertEquals(0, actualArcType.getIndex());
    assertArrayEquals(new String[]{"open", "chord", "pie"}, actualArcType.getValues());
  }

  /**
   * Test {@link Arc#executeDrawOperation()}.
   * <ul>
   *   <li>Given {@link Arc} (default constructor) addDraw {@link Draw} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Arc#executeDrawOperation()}
   */
  @Test
  public void testExecuteDrawOperation_givenArcAddDrawDraw() {
    // Arrange
    Arc arc = new Arc();
    arc.addDraw(new Draw());
    arc.addRotate(new Rotate());
    arc.setStrokewidth(2);

    // Act
    BufferedImage actualExecuteDrawOperationResult = arc.executeDrawOperation();

    // Assert
    SampleModel sampleModel = actualExecuteDrawOperationResult.getSampleModel();
    assertTrue(sampleModel instanceof PixelInterleavedSampleModel);
    assertArrayEquals(new int[]{0, 0, 0, 0}, ((PixelInterleavedSampleModel) sampleModel).getBankIndices());
    assertArrayEquals(new int[]{3, 2, 1, 0}, ((PixelInterleavedSampleModel) sampleModel).getBandOffsets());
    assertArrayEquals(new int[]{8, 8, 8, 8}, actualExecuteDrawOperationResult.getColorModel().getComponentSize());
    assertArrayEquals(new int[]{8, 8, 8, 8}, sampleModel.getSampleSize());
  }

  /**
   * Test {@link Arc#executeDrawOperation()}.
   * <ul>
   *   <li>Given {@link Arc} (default constructor) addRotate {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Arc#executeDrawOperation()}
   */
  @Test
  public void testExecuteDrawOperation_givenArcAddRotateNull() {
    // Arrange
    Arc arc = new Arc();
    arc.addRotate(null);
    arc.setStrokewidth(2);

    // Act
    BufferedImage actualExecuteDrawOperationResult = arc.executeDrawOperation();

    // Assert
    SampleModel sampleModel = actualExecuteDrawOperationResult.getSampleModel();
    assertTrue(sampleModel instanceof PixelInterleavedSampleModel);
    assertArrayEquals(new int[]{0, 0, 0, 0}, ((PixelInterleavedSampleModel) sampleModel).getBankIndices());
    assertArrayEquals(new int[]{3, 2, 1, 0}, ((PixelInterleavedSampleModel) sampleModel).getBandOffsets());
    assertArrayEquals(new int[]{8, 8, 8, 8}, actualExecuteDrawOperationResult.getColorModel().getComponentSize());
    assertArrayEquals(new int[]{8, 8, 8, 8}, sampleModel.getSampleSize());
  }

  /**
   * Test {@link Arc#executeDrawOperation()}.
   * <ul>
   *   <li>Given {@link Arc} (default constructor) Stroke is {@code Col}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Arc#executeDrawOperation()}
   */
  @Test
  public void testExecuteDrawOperation_givenArcStrokeIsCol() {
    // Arrange
    Arc arc = new Arc();
    arc.setStroke("Col");
    arc.addRotate(new Rotate());
    arc.setStrokewidth(2);

    // Act
    BufferedImage actualExecuteDrawOperationResult = arc.executeDrawOperation();

    // Assert
    SampleModel sampleModel = actualExecuteDrawOperationResult.getSampleModel();
    assertTrue(sampleModel instanceof PixelInterleavedSampleModel);
    assertArrayEquals(new int[]{0, 0, 0, 0}, ((PixelInterleavedSampleModel) sampleModel).getBankIndices());
    assertArrayEquals(new int[]{3, 2, 1, 0}, ((PixelInterleavedSampleModel) sampleModel).getBandOffsets());
    assertArrayEquals(new int[]{8, 8, 8, 8}, actualExecuteDrawOperationResult.getColorModel().getComponentSize());
    assertArrayEquals(new int[]{8, 8, 8, 8}, sampleModel.getSampleSize());
  }

  /**
   * Test {@link Arc#executeDrawOperation()}.
   * <ul>
   *   <li>Given {@link Arc} (default constructor) Stroke is {@code transparent}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Arc#executeDrawOperation()}
   */
  @Test
  public void testExecuteDrawOperation_givenArcStrokeIsTransparent() {
    // Arrange
    Arc arc = new Arc();
    arc.setStroke("transparent");
    arc.addRotate(new Rotate());
    arc.setStrokewidth(2);

    // Act
    BufferedImage actualExecuteDrawOperationResult = arc.executeDrawOperation();

    // Assert
    SampleModel sampleModel = actualExecuteDrawOperationResult.getSampleModel();
    assertTrue(sampleModel instanceof PixelInterleavedSampleModel);
    assertArrayEquals(new int[]{0, 0, 0, 0}, ((PixelInterleavedSampleModel) sampleModel).getBankIndices());
    assertArrayEquals(new int[]{3, 2, 1, 0}, ((PixelInterleavedSampleModel) sampleModel).getBandOffsets());
    assertArrayEquals(new int[]{8, 8, 8, 8}, actualExecuteDrawOperationResult.getColorModel().getComponentSize());
    assertArrayEquals(new int[]{8, 8, 8, 8}, sampleModel.getSampleSize());
  }

  /**
   * Test {@link Arc#executeDrawOperation()}.
   * <ul>
   *   <li>Given {@link Text} (default constructor) String is {@code transparent}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Arc#executeDrawOperation()}
   */
  @Test
  public void testExecuteDrawOperation_givenTextStringIsTransparent() {
    // Arrange
    Text text = new Text();
    text.setString("transparent");

    Draw instr = new Draw();
    instr.addText(text);

    Arc arc = new Arc();
    arc.addDraw(instr);
    arc.addRotate(new Rotate());
    arc.setStrokewidth(2);

    // Act
    BufferedImage actualExecuteDrawOperationResult = arc.executeDrawOperation();

    // Assert
    SampleModel sampleModel = actualExecuteDrawOperationResult.getSampleModel();
    assertTrue(sampleModel instanceof PixelInterleavedSampleModel);
    assertArrayEquals(new int[]{0, 0, 0, 0}, ((PixelInterleavedSampleModel) sampleModel).getBankIndices());
    assertArrayEquals(new int[]{3, 2, 1, 0}, ((PixelInterleavedSampleModel) sampleModel).getBandOffsets());
    assertArrayEquals(new int[]{8, 8, 8, 8}, actualExecuteDrawOperationResult.getColorModel().getComponentSize());
    assertArrayEquals(new int[]{8, 8, 8, 8}, sampleModel.getSampleSize());
  }

  /**
   * Test {@link Arc#executeDrawOperation()}.
   * <ul>
   *   <li>Then SampleModel return {@link PixelInterleavedSampleModel}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Arc#executeDrawOperation()}
   */
  @Test
  public void testExecuteDrawOperation_thenSampleModelReturnPixelInterleavedSampleModel() {
    // Arrange
    Arc arc = new Arc();
    arc.setStrokewidth(2);

    // Act
    BufferedImage actualExecuteDrawOperationResult = arc.executeDrawOperation();

    // Assert
    SampleModel sampleModel = actualExecuteDrawOperationResult.getSampleModel();
    assertTrue(sampleModel instanceof PixelInterleavedSampleModel);
    assertArrayEquals(new int[]{0, 0, 0, 0}, ((PixelInterleavedSampleModel) sampleModel).getBankIndices());
    assertArrayEquals(new int[]{3, 2, 1, 0}, ((PixelInterleavedSampleModel) sampleModel).getBandOffsets());
    assertArrayEquals(new int[]{8, 8, 8, 8}, actualExecuteDrawOperationResult.getColorModel().getComponentSize());
    assertArrayEquals(new int[]{8, 8, 8, 8}, sampleModel.getSampleSize());
  }

  /**
   * Test {@link Arc#executeDrawOperation()}.
   * <ul>
   *   <li>Then SampleModel return {@link PixelInterleavedSampleModel}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Arc#executeDrawOperation()}
   */
  @Test
  public void testExecuteDrawOperation_thenSampleModelReturnPixelInterleavedSampleModel2() {
    // Arrange
    Arc arc = new Arc();
    arc.addRotate(new Rotate());
    arc.setStrokewidth(2);

    // Act
    BufferedImage actualExecuteDrawOperationResult = arc.executeDrawOperation();

    // Assert
    SampleModel sampleModel = actualExecuteDrawOperationResult.getSampleModel();
    assertTrue(sampleModel instanceof PixelInterleavedSampleModel);
    assertArrayEquals(new int[]{0, 0, 0, 0}, ((PixelInterleavedSampleModel) sampleModel).getBankIndices());
    assertArrayEquals(new int[]{3, 2, 1, 0}, ((PixelInterleavedSampleModel) sampleModel).getBandOffsets());
    assertArrayEquals(new int[]{8, 8, 8, 8}, actualExecuteDrawOperationResult.getColorModel().getComponentSize());
    assertArrayEquals(new int[]{8, 8, 8, 8}, sampleModel.getSampleSize());
  }

  /**
   * Test new {@link Arc} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Arc}
   */
  @Test
  public void testNewArc() {
    // Arrange and Act
    Arc actualArc = new Arc();

    // Assert
    assertEquals("transparent", actualArc.fill);
    Location location = actualArc.getLocation();
    assertNull(location.getFileName());
    assertNull(actualArc.getDescription());
    assertNull(actualArc.getProject());
    assertNull(actualArc.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualArc.height);
    assertEquals(0, actualArc.strokeWidth);
    assertEquals(0, actualArc.width);
    assertFalse(actualArc.isReference());
    assertTrue(actualArc.instructions.isEmpty());
    assertEquals(ColorMapper.COLOR_BLACK, actualArc.stroke);
  }
}
