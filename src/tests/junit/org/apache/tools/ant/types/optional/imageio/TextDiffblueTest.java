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

public class TextDiffblueTest {
  /**
   * Test {@link Text#executeDrawOperation()}.
   * <ul>
   *   <li>Given {@link Text} (default constructor) Color is {@code Creating Text "}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Text#executeDrawOperation()}
   */
  @Test
  public void testExecuteDrawOperation_givenTextColorIsCreatingText() {
    // Arrange
    Text text = new Text();
    text.setColor("\tCreating Text \"");
    text.setString("\tCreating Text \"");

    // Act
    BufferedImage actualExecuteDrawOperationResult = text.executeDrawOperation();

    // Assert
    SampleModel sampleModel = actualExecuteDrawOperationResult.getSampleModel();
    assertTrue(sampleModel instanceof PixelInterleavedSampleModel);
    assertArrayEquals(new int[]{0, 0, 0, 0}, ((PixelInterleavedSampleModel) sampleModel).getBankIndices());
    assertArrayEquals(new int[]{3, 2, 1, 0}, ((PixelInterleavedSampleModel) sampleModel).getBandOffsets());
    assertArrayEquals(new int[]{8, 8, 8, 8}, actualExecuteDrawOperationResult.getColorModel().getComponentSize());
    assertArrayEquals(new int[]{8, 8, 8, 8}, sampleModel.getSampleSize());
  }

  /**
   * Test {@link Text#executeDrawOperation()}.
   * <ul>
   *   <li>Then SampleModel return {@link PixelInterleavedSampleModel}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Text#executeDrawOperation()}
   */
  @Test
  public void testExecuteDrawOperation_thenSampleModelReturnPixelInterleavedSampleModel() {
    // Arrange
    Text text = new Text();
    text.setString("\tCreating Text \"");

    // Act
    BufferedImage actualExecuteDrawOperationResult = text.executeDrawOperation();

    // Assert
    SampleModel sampleModel = actualExecuteDrawOperationResult.getSampleModel();
    assertTrue(sampleModel instanceof PixelInterleavedSampleModel);
    assertArrayEquals(new int[]{0, 0, 0, 0}, ((PixelInterleavedSampleModel) sampleModel).getBankIndices());
    assertArrayEquals(new int[]{3, 2, 1, 0}, ((PixelInterleavedSampleModel) sampleModel).getBandOffsets());
    assertArrayEquals(new int[]{8, 8, 8, 8}, actualExecuteDrawOperationResult.getColorModel().getComponentSize());
    assertArrayEquals(new int[]{8, 8, 8, 8}, sampleModel.getSampleSize());
  }

  /**
   * Test new {@link Text} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Text}
   */
  @Test
  public void testNewText() {
    // Arrange and Act
    Text actualText = new Text();

    // Assert
    Location location = actualText.getLocation();
    assertNull(location.getFileName());
    assertNull(actualText.getDescription());
    assertNull(actualText.getProject());
    assertNull(actualText.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(actualText.isReference());
    assertTrue(actualText.instructions.isEmpty());
  }
}
