package org.apache.tools.ant.types;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import org.junit.Test;

public class CharSetDiffblueTest {
  /**
   * Test {@link CharSet#CharSet()}.
   * <p>
   * Method under test: {@link CharSet#CharSet()}
   */
  @Test
  public void testNewCharSet() {
    // Arrange and Act
    CharSet actualCharSet = new CharSet();

    // Assert
    assertNull(actualCharSet.getValue());
    assertEquals(-1, actualCharSet.getIndex());
  }

  /**
   * Test {@link CharSet#CharSet(String)}.
   * <ul>
   *   <li>When {@code Big5}.</li>
   *   <li>Then return Value is {@code Big5}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CharSet#CharSet(String)}
   */
  @Test
  public void testNewCharSet_whenBig5_thenReturnValueIsBig5() {
    // Arrange and Act
    CharSet actualCharSet = new CharSet("Big5");

    // Assert
    assertEquals("Big5", actualCharSet.getValue());
    assertEquals(0, actualCharSet.getIndex());
    assertEquals(964, actualCharSet.getValues().length);
  }

  /**
   * Test {@link CharSet#CharSet(String)}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then return Value is {@code UTF-8}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CharSet#CharSet(String)}
   */
  @Test
  public void testNewCharSet_whenEmptyString_thenReturnValueIsUtf8() {
    // Arrange and Act
    CharSet actualCharSet = new CharSet("");

    // Assert
    assertEquals("UTF-8", actualCharSet.getValue());
    assertEquals(591, actualCharSet.getIndex());
    assertEquals(964, actualCharSet.getValues().length);
  }

  /**
   * Test {@link CharSet#CharSet(String)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return Value is {@code UTF-8}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CharSet#CharSet(String)}
   */
  @Test
  public void testNewCharSet_whenNull_thenReturnValueIsUtf8() {
    // Arrange and Act
    CharSet actualCharSet = new CharSet(null);

    // Assert
    assertEquals("UTF-8", actualCharSet.getValue());
    assertEquals(591, actualCharSet.getIndex());
    assertEquals(964, actualCharSet.getValues().length);
  }

  /**
   * Test {@link CharSet#getDefault()}.
   * <p>
   * Method under test: {@link CharSet#getDefault()}
   */
  @Test
  public void testGetDefault() {
    // Arrange and Act
    CharSet actualDefault = CharSet.getDefault();

    // Assert
    assertEquals("UTF-8", actualDefault.getValue());
    assertEquals(591, actualDefault.getIndex());
    assertEquals(964, actualDefault.getValues().length);
  }

  /**
   * Test {@link CharSet#getAscii()}.
   * <p>
   * Method under test: {@link CharSet#getAscii()}
   */
  @Test
  public void testGetAscii() {
    // Arrange and Act
    CharSet actualAscii = CharSet.getAscii();

    // Assert
    assertEquals("US-ASCII", actualAscii.getValue());
    assertEquals(553, actualAscii.getIndex());
    assertEquals(964, actualAscii.getValues().length);
  }

  /**
   * Test {@link CharSet#getUtf8()}.
   * <p>
   * Method under test: {@link CharSet#getUtf8()}
   */
  @Test
  public void testGetUtf8() {
    // Arrange and Act
    CharSet actualUtf8 = CharSet.getUtf8();

    // Assert
    assertEquals("UTF-8", actualUtf8.getValue());
    assertEquals(591, actualUtf8.getIndex());
    assertEquals(964, actualUtf8.getValues().length);
  }

  /**
   * Test {@link CharSet#equivalent(CharSet)}.
   * <ul>
   *   <li>Given Ascii.</li>
   *   <li>When Ascii.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CharSet#equivalent(CharSet)}
   */
  @Test
  public void testEquivalent_givenAscii_whenAscii_thenReturnTrue() {
    // Arrange
    CharSet ascii = CharSet.getAscii();

    // Act and Assert
    assertTrue(ascii.equivalent(CharSet.getAscii()));
  }

  /**
   * Test {@link CharSet#equivalent(CharSet)}.
   * <ul>
   *   <li>Given Default.</li>
   *   <li>When Ascii.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CharSet#equivalent(CharSet)}
   */
  @Test
  public void testEquivalent_givenDefault_whenAscii_thenReturnFalse() {
    // Arrange
    CharSet resultDefault = CharSet.getDefault();

    // Act and Assert
    assertFalse(resultDefault.equivalent(CharSet.getAscii()));
  }

  /**
   * Test {@link CharSet#getValues()}.
   * <p>
   * Method under test: {@link CharSet#getValues()}
   */
  @Test
  public void testGetValues() {
    // Arrange and Act
    String[] actualValues = CharSet.getAscii().getValues();

    // Assert
    assertEquals("5601", actualValues[23]);
    assertEquals("Big5", actualValues[0]);
    assertEquals("Big5-HKSCS", actualValues[2]);
    assertEquals("Big5_HKSCS", actualValues[5]);
    assertEquals("CESU-8", actualValues[7]);
    assertEquals("CESU8", actualValues[8]);
    assertEquals("EUC-JP", actualValues[10]);
    assertEquals("EUC-KR", actualValues[18]);
    assertEquals("Extended_UNIX_Code_Packed_Format_for_Japanese", actualValues[14]);
    assertEquals("UTF-32BE-BOM", actualValues[940]);
    assertEquals("UTF-32LE-BOM", actualValues[943]);
    assertEquals("UTF_32BE_BOM", actualValues[939]);
    assertEquals("UTF_32LE_BOM", actualValues[942]);
    assertEquals("X-UTF-32LE-BOM", actualValues[941]);
    assertEquals("big5-hkscs", actualValues[3]);
    assertEquals("big5hk", actualValues[4]);
    assertEquals("big5hkscs", actualValues[6]);
    assertEquals("cp50220", actualValues[945]);
    assertEquals("cp50221", actualValues[948]);
    assertEquals("csBig5", actualValues[1]);
    assertEquals("csCESU-8", actualValues[9]);
    assertEquals("csEUCKR", actualValues[20]);
    assertEquals("csEUCPkdFmtjapanese", actualValues[11]);
    assertEquals("euc_jp", actualValues[15]);
    assertEquals("euc_kr", actualValues[24]);
    assertEquals("eucjis", actualValues[13]);
    assertEquals("eucjp", actualValues[Short.SIZE]);
    assertEquals("ksc5601", actualValues[22]);
    assertEquals("ksc5601-1987", actualValues[19]);
    assertEquals("ksc5601_1987", actualValues[21]);
    assertEquals("ms-874", actualValues[951]);
    assertEquals("ms50220", actualValues[946]);
    assertEquals("ms50221", actualValues[949]);
    assertEquals("ms874", actualValues[952]);
    assertEquals("ms949", actualValues[956]);
    assertEquals("ms950", actualValues[960]);
    assertEquals("ms_949", actualValues[958]);
    assertEquals("windows-874", actualValues[953]);
    assertEquals("windows-949", actualValues[957]);
    assertEquals("windows-950", actualValues[961]);
    assertEquals("windows-iso2022jp", actualValues[963]);
    assertEquals("windows949", actualValues[955]);
    assertEquals("x-euc-jp", actualValues[12]);
    assertEquals("x-eucjp", actualValues[17]);
    assertEquals("x-windows-50220", actualValues[944]);
    assertEquals("x-windows-50221", actualValues[947]);
    assertEquals("x-windows-874", actualValues[950]);
    assertEquals("x-windows-949", actualValues[954]);
    assertEquals("x-windows-950", actualValues[959]);
    assertEquals("x-windows-iso2022jp", actualValues[962]);
    assertEquals(964, actualValues.length);
  }

  /**
   * Test {@link CharSet#setValue(String)}.
   * <ul>
   *   <li>When {@code Big5}.</li>
   *   <li>Then Ascii Charset name is {@code Big5}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CharSet#setValue(String)}
   */
  @Test
  public void testSetValue_whenBig5_thenAsciiCharsetNameIsBig5() {
    // Arrange
    CharSet ascii = CharSet.getAscii();

    // Act
    ascii.setValue("Big5");

    // Assert
    assertEquals("Big5", ascii.getCharset().name());
    assertEquals("Big5", ascii.getValue());
    assertEquals(0, ascii.getIndex());
  }

  /**
   * Test {@link CharSet#setValue(String)}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then Ascii Charset name is {@code UTF-8}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CharSet#setValue(String)}
   */
  @Test
  public void testSetValue_whenEmptyString_thenAsciiCharsetNameIsUtf8() {
    // Arrange
    CharSet ascii = CharSet.getAscii();

    // Act
    ascii.setValue("");

    // Assert
    assertEquals("UTF-8", ascii.getCharset().name());
    assertEquals("UTF-8", ascii.getValue());
    assertEquals(591, ascii.getIndex());
  }

  /**
   * Test {@link CharSet#setValue(String)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then Ascii Charset name is {@code UTF-8}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CharSet#setValue(String)}
   */
  @Test
  public void testSetValue_whenNull_thenAsciiCharsetNameIsUtf8() {
    // Arrange
    CharSet ascii = CharSet.getAscii();

    // Act
    ascii.setValue(null);

    // Assert
    assertEquals("UTF-8", ascii.getCharset().name());
    assertEquals("UTF-8", ascii.getValue());
    assertEquals(591, ascii.getIndex());
  }
}
