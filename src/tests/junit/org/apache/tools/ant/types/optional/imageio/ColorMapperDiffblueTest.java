package org.apache.tools.ant.types.optional.imageio;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import java.awt.Color;
import java.awt.color.ColorSpace;
import java.awt.color.ICC_ColorSpace;
import java.awt.color.ICC_Profile;
import java.awt.color.ICC_ProfileRGB;
import org.junit.Test;

public class ColorMapperDiffblueTest {
  /**
   * Test {@link ColorMapper#getColorByName(String)}.
   * <ul>
   *   <li>When {@code Color Name}.</li>
   *   <li>Then return brighter brighter darker.</li>
   * </ul>
   * <p>
   * Method under test: {@link ColorMapper#getColorByName(String)}
   */
  @Test
  public void testGetColorByName_whenColorName_thenReturnBrighterBrighterDarker() {
    // Arrange and Act
    Color actualColorByName = ColorMapper.getColorByName("Color Name");

    // Assert
    ColorSpace colorSpace = actualColorByName.getColorSpace();
    assertTrue(colorSpace instanceof ICC_ColorSpace);
    ICC_Profile profile = ((ICC_ColorSpace) colorSpace).getProfile();
    assertTrue(profile instanceof ICC_ProfileRGB);
    Color brighterResult = actualColorByName.brighter();
    Color brighterResult2 = brighterResult.brighter();
    assertEquals(brighterResult2.darker(), brighterResult2.darker());
    Color expectedDarkerResult = actualColorByName.black;
    assertEquals(expectedDarkerResult, actualColorByName.darker());
    assertSame(colorSpace, brighterResult2.getColorSpace());
    assertSame(colorSpace, brighterResult.getColorSpace());
    assertSame(colorSpace, brighterResult.darker().getColorSpace());
    assertArrayEquals(new float[]{0.95014954f, 1.0f, 1.0882568f}, ((ICC_ProfileRGB) profile).getMediaWhitePoint(),
        0.0f);
  }

  /**
   * Test {@link ColorMapper#getColorByName(String)}.
   * <ul>
   *   <li>When {@link ColorMapper#COLOR_BLACK}.</li>
   *   <li>Then return brighter brighter darker.</li>
   * </ul>
   * <p>
   * Method under test: {@link ColorMapper#getColorByName(String)}
   */
  @Test
  public void testGetColorByName_whenColor_black_thenReturnBrighterBrighterDarker() {
    // Arrange and Act
    Color actualColorByName = ColorMapper.getColorByName(ColorMapper.COLOR_BLACK);

    // Assert
    ColorSpace colorSpace = actualColorByName.getColorSpace();
    assertTrue(colorSpace instanceof ICC_ColorSpace);
    ICC_Profile profile = ((ICC_ColorSpace) colorSpace).getProfile();
    assertTrue(profile instanceof ICC_ProfileRGB);
    Color brighterResult = actualColorByName.brighter();
    Color brighterResult2 = brighterResult.brighter();
    assertEquals(brighterResult2.darker(), brighterResult2.darker());
    Color expectedDarkerResult = actualColorByName.black;
    assertEquals(expectedDarkerResult, actualColorByName.darker());
    assertSame(colorSpace, brighterResult2.getColorSpace());
    assertSame(colorSpace, brighterResult.getColorSpace());
    assertSame(colorSpace, brighterResult.darker().getColorSpace());
    assertArrayEquals(new float[]{0.95014954f, 1.0f, 1.0882568f}, ((ICC_ProfileRGB) profile).getMediaWhitePoint(),
        0.0f);
  }

  /**
   * Test {@link ColorMapper#getColorByName(String)}.
   * <ul>
   *   <li>When {@link ColorMapper#COLOR_BLUE}.</li>
   *   <li>Then return RGB is {@code -16776961}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ColorMapper#getColorByName(String)}
   */
  @Test
  public void testGetColorByName_whenColor_blue_thenReturnRgbIs16776961() {
    // Arrange and Act
    Color actualColorByName = ColorMapper.getColorByName(ColorMapper.COLOR_BLUE);

    // Assert
    assertEquals(-16776961, actualColorByName.getRGB());
    assertEquals(-16777038, actualColorByName.darker().getRGB());
    Color expectedBrighterResult = actualColorByName.blue;
    assertEquals(expectedBrighterResult, actualColorByName.brighter());
  }

  /**
   * Test {@link ColorMapper#getColorByName(String)}.
   * <ul>
   *   <li>When {@link ColorMapper#COLOR_CYAN}.</li>
   *   <li>Then return RGB is {@code -16711681}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ColorMapper#getColorByName(String)}
   */
  @Test
  public void testGetColorByName_whenColor_cyan_thenReturnRgbIs16711681() {
    // Arrange and Act
    Color actualColorByName = ColorMapper.getColorByName(ColorMapper.COLOR_CYAN);

    // Assert
    assertEquals(-16711681, actualColorByName.getRGB());
    assertEquals(-16731470, actualColorByName.darker().getRGB());
    Color expectedBrighterResult = actualColorByName.cyan;
    assertEquals(expectedBrighterResult, actualColorByName.brighter());
  }

  /**
   * Test {@link ColorMapper#getColorByName(String)}.
   * <ul>
   *   <li>When {@link ColorMapper#COLOR_DARKGRAY}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ColorMapper#getColorByName(String)}
   */
  @Test
  public void testGetColorByName_whenColor_darkgray() {
    // Arrange and Act
    Color actualColorByName = ColorMapper.getColorByName(ColorMapper.COLOR_DARKGRAY);

    // Assert
    ColorSpace colorSpace = actualColorByName.getColorSpace();
    assertTrue(colorSpace instanceof ICC_ColorSpace);
    ICC_Profile profile = ((ICC_ColorSpace) colorSpace).getProfile();
    assertTrue(profile instanceof ICC_ProfileRGB);
    Color brighterResult = actualColorByName.brighter();
    assertSame(colorSpace, brighterResult.brighter().getColorSpace());
    Color darkerResult = actualColorByName.darker();
    assertSame(colorSpace, darkerResult.brighter().getColorSpace());
    assertSame(colorSpace, brighterResult.getColorSpace());
    assertSame(colorSpace, brighterResult.darker().getColorSpace());
    assertSame(colorSpace, darkerResult.darker().getColorSpace());
    assertSame(colorSpace, darkerResult.getColorSpace());
    assertArrayEquals(new float[]{0.95014954f, 1.0f, 1.0882568f}, ((ICC_ProfileRGB) profile).getMediaWhitePoint(),
        0.0f);
  }

  /**
   * Test {@link ColorMapper#getColorByName(String)}.
   * <ul>
   *   <li>When {@link ColorMapper#COLOR_DARKGREY}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ColorMapper#getColorByName(String)}
   */
  @Test
  public void testGetColorByName_whenColor_darkgrey() {
    // Arrange and Act
    Color actualColorByName = ColorMapper.getColorByName(ColorMapper.COLOR_DARKGREY);

    // Assert
    ColorSpace colorSpace = actualColorByName.getColorSpace();
    assertTrue(colorSpace instanceof ICC_ColorSpace);
    ICC_Profile profile = ((ICC_ColorSpace) colorSpace).getProfile();
    assertTrue(profile instanceof ICC_ProfileRGB);
    Color brighterResult = actualColorByName.brighter();
    assertSame(colorSpace, brighterResult.brighter().getColorSpace());
    Color darkerResult = actualColorByName.darker();
    assertSame(colorSpace, darkerResult.brighter().getColorSpace());
    assertSame(colorSpace, brighterResult.getColorSpace());
    assertSame(colorSpace, brighterResult.darker().getColorSpace());
    assertSame(colorSpace, darkerResult.darker().getColorSpace());
    assertSame(colorSpace, darkerResult.getColorSpace());
    assertArrayEquals(new float[]{0.95014954f, 1.0f, 1.0882568f}, ((ICC_ProfileRGB) profile).getMediaWhitePoint(),
        0.0f);
  }

  /**
   * Test {@link ColorMapper#getColorByName(String)}.
   * <ul>
   *   <li>When {@link ColorMapper#COLOR_GRAY}.</li>
   *   <li>Then return darker brighter is brighter darker.</li>
   * </ul>
   * <p>
   * Method under test: {@link ColorMapper#getColorByName(String)}
   */
  @Test
  public void testGetColorByName_whenColor_gray_thenReturnDarkerBrighterIsBrighterDarker() {
    // Arrange and Act
    Color actualColorByName = ColorMapper.getColorByName(ColorMapper.COLOR_GRAY);

    // Assert
    ColorSpace colorSpace = actualColorByName.getColorSpace();
    assertTrue(colorSpace instanceof ICC_ColorSpace);
    ICC_Profile profile = ((ICC_ColorSpace) colorSpace).getProfile();
    assertTrue(profile instanceof ICC_ProfileRGB);
    Color brighterResult = actualColorByName.brighter();
    Color darkerResult = brighterResult.darker();
    Color darkerResult2 = actualColorByName.darker();
    assertEquals(darkerResult, darkerResult2.brighter());
    Color expectedBrighterResult = actualColorByName.white;
    assertEquals(expectedBrighterResult, brighterResult.brighter());
    assertSame(colorSpace, brighterResult.getColorSpace());
    assertSame(colorSpace, darkerResult.getColorSpace());
    assertSame(colorSpace, darkerResult2.darker().getColorSpace());
    assertSame(colorSpace, darkerResult2.getColorSpace());
    assertArrayEquals(new float[]{0.95014954f, 1.0f, 1.0882568f}, ((ICC_ProfileRGB) profile).getMediaWhitePoint(),
        0.0f);
  }

  /**
   * Test {@link ColorMapper#getColorByName(String)}.
   * <ul>
   *   <li>When {@link ColorMapper#COLOR_GREEN}.</li>
   *   <li>Then return RGB is {@code -16711936}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ColorMapper#getColorByName(String)}
   */
  @Test
  public void testGetColorByName_whenColor_green_thenReturnRgbIs16711936() {
    // Arrange and Act
    Color actualColorByName = ColorMapper.getColorByName(ColorMapper.COLOR_GREEN);

    // Assert
    assertEquals(-16711936, actualColorByName.getRGB());
    assertEquals(-16731648, actualColorByName.darker().getRGB());
    Color expectedBrighterResult = actualColorByName.green;
    assertEquals(expectedBrighterResult, actualColorByName.brighter());
  }

  /**
   * Test {@link ColorMapper#getColorByName(String)}.
   * <ul>
   *   <li>When {@link ColorMapper#COLOR_GREY}.</li>
   *   <li>Then return darker brighter is brighter darker.</li>
   * </ul>
   * <p>
   * Method under test: {@link ColorMapper#getColorByName(String)}
   */
  @Test
  public void testGetColorByName_whenColor_grey_thenReturnDarkerBrighterIsBrighterDarker() {
    // Arrange and Act
    Color actualColorByName = ColorMapper.getColorByName(ColorMapper.COLOR_GREY);

    // Assert
    ColorSpace colorSpace = actualColorByName.getColorSpace();
    assertTrue(colorSpace instanceof ICC_ColorSpace);
    ICC_Profile profile = ((ICC_ColorSpace) colorSpace).getProfile();
    assertTrue(profile instanceof ICC_ProfileRGB);
    Color brighterResult = actualColorByName.brighter();
    Color darkerResult = brighterResult.darker();
    Color darkerResult2 = actualColorByName.darker();
    assertEquals(darkerResult, darkerResult2.brighter());
    Color expectedBrighterResult = actualColorByName.white;
    assertEquals(expectedBrighterResult, brighterResult.brighter());
    assertSame(colorSpace, brighterResult.getColorSpace());
    assertSame(colorSpace, darkerResult.getColorSpace());
    assertSame(colorSpace, darkerResult2.darker().getColorSpace());
    assertSame(colorSpace, darkerResult2.getColorSpace());
    assertArrayEquals(new float[]{0.95014954f, 1.0f, 1.0882568f}, ((ICC_ProfileRGB) profile).getMediaWhitePoint(),
        0.0f);
  }

  /**
   * Test {@link ColorMapper#getColorByName(String)}.
   * <ul>
   *   <li>When {@link ColorMapper#COLOR_LIGHTGRAY}.</li>
   *   <li>Then return brighter is {@link Color#white}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ColorMapper#getColorByName(String)}
   */
  @Test
  public void testGetColorByName_whenColor_lightgray_thenReturnBrighterIsWhite() {
    // Arrange and Act
    Color actualColorByName = ColorMapper.getColorByName(ColorMapper.COLOR_LIGHTGRAY);

    // Assert
    ColorSpace colorSpace = actualColorByName.getColorSpace();
    assertTrue(colorSpace instanceof ICC_ColorSpace);
    ICC_Profile profile = ((ICC_ColorSpace) colorSpace).getProfile();
    assertTrue(profile instanceof ICC_ProfileRGB);
    Color expectedBrighterResult = actualColorByName.white;
    assertEquals(expectedBrighterResult, actualColorByName.brighter());
    Color darkerResult = actualColorByName.darker();
    assertSame(colorSpace, darkerResult.brighter().getColorSpace());
    assertSame(colorSpace, darkerResult.darker().getColorSpace());
    assertSame(colorSpace, darkerResult.getColorSpace());
    assertArrayEquals(new float[]{0.95014954f, 1.0f, 1.0882568f}, ((ICC_ProfileRGB) profile).getMediaWhitePoint(),
        0.0f);
  }

  /**
   * Test {@link ColorMapper#getColorByName(String)}.
   * <ul>
   *   <li>When {@link ColorMapper#COLOR_LIGHTGREY}.</li>
   *   <li>Then return brighter is {@link Color#white}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ColorMapper#getColorByName(String)}
   */
  @Test
  public void testGetColorByName_whenColor_lightgrey_thenReturnBrighterIsWhite() {
    // Arrange and Act
    Color actualColorByName = ColorMapper.getColorByName(ColorMapper.COLOR_LIGHTGREY);

    // Assert
    ColorSpace colorSpace = actualColorByName.getColorSpace();
    assertTrue(colorSpace instanceof ICC_ColorSpace);
    ICC_Profile profile = ((ICC_ColorSpace) colorSpace).getProfile();
    assertTrue(profile instanceof ICC_ProfileRGB);
    Color expectedBrighterResult = actualColorByName.white;
    assertEquals(expectedBrighterResult, actualColorByName.brighter());
    Color darkerResult = actualColorByName.darker();
    assertSame(colorSpace, darkerResult.brighter().getColorSpace());
    assertSame(colorSpace, darkerResult.darker().getColorSpace());
    assertSame(colorSpace, darkerResult.getColorSpace());
    assertArrayEquals(new float[]{0.95014954f, 1.0f, 1.0882568f}, ((ICC_ProfileRGB) profile).getMediaWhitePoint(),
        0.0f);
  }

  /**
   * Test {@link ColorMapper#getColorByName(String)}.
   * <ul>
   *   <li>When {@link ColorMapper#COLOR_MAGENTA}.</li>
   *   <li>Then return darker RGB is {@code -5111630}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ColorMapper#getColorByName(String)}
   */
  @Test
  public void testGetColorByName_whenColor_magenta_thenReturnDarkerRgbIs5111630() {
    // Arrange and Act
    Color actualColorByName = ColorMapper.getColorByName(ColorMapper.COLOR_MAGENTA);

    // Assert
    assertEquals(-5111630, actualColorByName.darker().getRGB());
    assertEquals(-65281, actualColorByName.getRGB());
    Color expectedBrighterResult = actualColorByName.magenta;
    assertEquals(expectedBrighterResult, actualColorByName.brighter());
  }

  /**
   * Test {@link ColorMapper#getColorByName(String)}.
   * <ul>
   *   <li>When {@link ColorMapper#COLOR_ORANGE}.</li>
   *   <li>Then return RGB is {@code -14336}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ColorMapper#getColorByName(String)}
   */
  @Test
  public void testGetColorByName_whenColor_orange_thenReturnRgbIs14336() {
    // Arrange and Act
    Color actualColorByName = ColorMapper.getColorByName(ColorMapper.COLOR_ORANGE);

    // Assert
    assertEquals(-14336, actualColorByName.getRGB());
    Color darkerResult = actualColorByName.darker();
    assertEquals(-5075968, darkerResult.getRGB());
    assertEquals(140, darkerResult.getGreen());
    assertEquals(200, actualColorByName.getGreen());
    Color expectedBrighterResult = actualColorByName.yellow;
    assertEquals(expectedBrighterResult, actualColorByName.brighter());
  }

  /**
   * Test {@link ColorMapper#getColorByName(String)}.
   * <ul>
   *   <li>When {@link ColorMapper#COLOR_PINK}.</li>
   *   <li>Then return brighter RGB is {@code -1286}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ColorMapper#getColorByName(String)}
   */
  @Test
  public void testGetColorByName_whenColor_pink_thenReturnBrighterRgbIs1286() {
    // Arrange and Act
    Color actualColorByName = ColorMapper.getColorByName(ColorMapper.COLOR_PINK);

    // Assert
    Color brighterResult = actualColorByName.brighter();
    assertEquals(-1286, brighterResult.getRGB());
    assertEquals(-20561, actualColorByName.getRGB());
    Color darkerResult = actualColorByName.darker();
    assertEquals(-5080454, darkerResult.getRGB());
    assertEquals(122, darkerResult.getBlue());
    assertEquals(122, darkerResult.getGreen());
    assertEquals(175, actualColorByName.getBlue());
    assertEquals(175, actualColorByName.getGreen());
    assertEquals(250, brighterResult.getBlue());
    assertEquals(250, brighterResult.getGreen());
    Color expectedBrighterResult = actualColorByName.white;
    assertEquals(expectedBrighterResult, brighterResult.brighter());
  }

  /**
   * Test {@link ColorMapper#getColorByName(String)}.
   * <ul>
   *   <li>When {@link ColorMapper#COLOR_RED}.</li>
   *   <li>Then return darker RGB is {@code -5111808}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ColorMapper#getColorByName(String)}
   */
  @Test
  public void testGetColorByName_whenColor_red_thenReturnDarkerRgbIs5111808() {
    // Arrange and Act
    Color actualColorByName = ColorMapper.getColorByName(ColorMapper.COLOR_RED);

    // Assert
    assertEquals(-5111808, actualColorByName.darker().getRGB());
    assertEquals(-65536, actualColorByName.getRGB());
    Color expectedBrighterResult = actualColorByName.red;
    assertEquals(expectedBrighterResult, actualColorByName.brighter());
  }

  /**
   * Test {@link ColorMapper#getColorByName(String)}.
   * <ul>
   *   <li>When {@link ColorMapper#COLOR_WHITE}.</li>
   *   <li>Then return RGB is minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link ColorMapper#getColorByName(String)}
   */
  @Test
  public void testGetColorByName_whenColor_white_thenReturnRgbIsMinusOne() {
    // Arrange and Act
    Color actualColorByName = ColorMapper.getColorByName(ColorMapper.COLOR_WHITE);

    // Assert
    assertEquals(-1, actualColorByName.getRGB());
    assertEquals(-5066062, actualColorByName.darker().getRGB());
    Color expectedBrighterResult = actualColorByName.white;
    assertEquals(expectedBrighterResult, actualColorByName.brighter());
  }

  /**
   * Test {@link ColorMapper#getColorByName(String)}.
   * <ul>
   *   <li>When {@link ColorMapper#COLOR_YELLOW}.</li>
   *   <li>Then return RGB is minus two hundred fifty-six.</li>
   * </ul>
   * <p>
   * Method under test: {@link ColorMapper#getColorByName(String)}
   */
  @Test
  public void testGetColorByName_whenColor_yellow_thenReturnRgbIsMinusTwoHundredFiftySix() {
    // Arrange and Act
    Color actualColorByName = ColorMapper.getColorByName(ColorMapper.COLOR_YELLOW);

    // Assert
    assertEquals(-256, actualColorByName.getRGB());
    assertEquals(-5066240, actualColorByName.darker().getRGB());
    Color expectedBrighterResult = actualColorByName.yellow;
    assertEquals(expectedBrighterResult, actualColorByName.brighter());
  }
}
