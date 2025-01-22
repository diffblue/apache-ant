package org.apache.tools.zip;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertTrue;
import java.io.UnsupportedEncodingException;
import org.junit.Test;

public class GeneralPurposeBitDiffblueTest {
  /**
   * Test {@link GeneralPurposeBit#usesUTF8ForNames()}.
   * <ul>
   *   <li>Given {@link GeneralPurposeBit} (default constructor) useUTF8ForNames {@code true}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link GeneralPurposeBit#usesUTF8ForNames()}
   */
  @Test
  public void testUsesUTF8ForNames_givenGeneralPurposeBitUseUTF8ForNamesTrue_thenReturnTrue() {
    // Arrange
    GeneralPurposeBit generalPurposeBit = new GeneralPurposeBit();
    generalPurposeBit.useUTF8ForNames(true);

    // Act and Assert
    assertTrue(generalPurposeBit.usesUTF8ForNames());
  }

  /**
   * Test {@link GeneralPurposeBit#usesUTF8ForNames()}.
   * <ul>
   *   <li>Given {@link GeneralPurposeBit} (default constructor).</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link GeneralPurposeBit#usesUTF8ForNames()}
   */
  @Test
  public void testUsesUTF8ForNames_givenGeneralPurposeBit_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new GeneralPurposeBit()).usesUTF8ForNames());
  }

  /**
   * Test {@link GeneralPurposeBit#usesDataDescriptor()}.
   * <ul>
   *   <li>Given {@link GeneralPurposeBit} (default constructor).</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link GeneralPurposeBit#usesDataDescriptor()}
   */
  @Test
  public void testUsesDataDescriptor_givenGeneralPurposeBit_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new GeneralPurposeBit()).usesDataDescriptor());
  }

  /**
   * Test {@link GeneralPurposeBit#usesDataDescriptor()}.
   * <ul>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link GeneralPurposeBit#usesDataDescriptor()}
   */
  @Test
  public void testUsesDataDescriptor_thenReturnTrue() {
    // Arrange
    GeneralPurposeBit generalPurposeBit = new GeneralPurposeBit();
    generalPurposeBit.useDataDescriptor(true);

    // Act and Assert
    assertTrue(generalPurposeBit.usesDataDescriptor());
  }

  /**
   * Test {@link GeneralPurposeBit#usesEncryption()}.
   * <ul>
   *   <li>Given {@code A}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link GeneralPurposeBit#usesEncryption()}
   */
  @Test
  public void testUsesEncryption_givenA_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(GeneralPurposeBit.parse(new byte[]{'A', 3, 'A', 3, 'A', 3, 'A', 3}, 2).usesEncryption());
  }

  /**
   * Test {@link GeneralPurposeBit#usesEncryption()}.
   * <ul>
   *   <li>Given {@link GeneralPurposeBit} (default constructor).</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link GeneralPurposeBit#usesEncryption()}
   */
  @Test
  public void testUsesEncryption_givenGeneralPurposeBit_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new GeneralPurposeBit()).usesEncryption());
  }

  /**
   * Test {@link GeneralPurposeBit#usesStrongEncryption()}.
   * <ul>
   *   <li>Given {@link GeneralPurposeBit} (default constructor) useEncryption {@code true}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link GeneralPurposeBit#usesStrongEncryption()}
   */
  @Test
  public void testUsesStrongEncryption_givenGeneralPurposeBitUseEncryptionTrue_thenReturnFalse() {
    // Arrange
    GeneralPurposeBit generalPurposeBit = new GeneralPurposeBit();
    generalPurposeBit.useEncryption(true);

    // Act and Assert
    assertFalse(generalPurposeBit.usesStrongEncryption());
  }

  /**
   * Test {@link GeneralPurposeBit#usesStrongEncryption()}.
   * <ul>
   *   <li>Given {@link GeneralPurposeBit} (default constructor).</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link GeneralPurposeBit#usesStrongEncryption()}
   */
  @Test
  public void testUsesStrongEncryption_givenGeneralPurposeBit_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new GeneralPurposeBit()).usesStrongEncryption());
  }

  /**
   * Test {@link GeneralPurposeBit#usesStrongEncryption()}.
   * <ul>
   *   <li>Given parse {@code AXAXAXAX} Bytes is {@code UTF-8} and two.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link GeneralPurposeBit#usesStrongEncryption()}
   */
  @Test
  public void testUsesStrongEncryption_givenParseAxaxaxaxBytesIsUtf8AndTwo_thenReturnTrue()
      throws UnsupportedEncodingException {
    // Arrange, Act and Assert
    assertTrue(GeneralPurposeBit.parse("AXAXAXAX".getBytes("UTF-8"), 2).usesStrongEncryption());
  }

  /**
   * Test {@link GeneralPurposeBit#useStrongEncryption(boolean)}.
   * <ul>
   *   <li>When {@code false}.</li>
   *   <li>Then not {@link GeneralPurposeBit} (default constructor) usesEncryption.</li>
   * </ul>
   * <p>
   * Method under test: {@link GeneralPurposeBit#useStrongEncryption(boolean)}
   */
  @Test
  public void testUseStrongEncryption_whenFalse_thenNotGeneralPurposeBitUsesEncryption() {
    // Arrange
    GeneralPurposeBit generalPurposeBit = new GeneralPurposeBit();

    // Act
    generalPurposeBit.useStrongEncryption(false);

    // Assert that nothing has changed
    assertFalse(generalPurposeBit.usesEncryption());
    assertArrayEquals(new byte[]{0, 0}, generalPurposeBit.encode());
  }

  /**
   * Test {@link GeneralPurposeBit#useStrongEncryption(boolean)}.
   * <ul>
   *   <li>When {@code true}.</li>
   *   <li>Then {@link GeneralPurposeBit} (default constructor) usesEncryption.</li>
   * </ul>
   * <p>
   * Method under test: {@link GeneralPurposeBit#useStrongEncryption(boolean)}
   */
  @Test
  public void testUseStrongEncryption_whenTrue_thenGeneralPurposeBitUsesEncryption() {
    // Arrange
    GeneralPurposeBit generalPurposeBit = new GeneralPurposeBit();

    // Act
    generalPurposeBit.useStrongEncryption(true);

    // Assert
    assertTrue(generalPurposeBit.usesEncryption());
    assertArrayEquals(new byte[]{'A', 0}, generalPurposeBit.encode());
  }

  /**
   * Test {@link GeneralPurposeBit#encode(byte[], int)} with {@code byte[]}, {@code int}.
   * <ul>
   *   <li>Given {@link GeneralPurposeBit} (default constructor) useDataDescriptor {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link GeneralPurposeBit#encode(byte[], int)}
   */
  @Test
  public void testEncodeWithByteInt_givenGeneralPurposeBitUseDataDescriptorTrue() throws UnsupportedEncodingException {
    // Arrange
    GeneralPurposeBit generalPurposeBit = new GeneralPurposeBit();
    generalPurposeBit.useDataDescriptor(true);
    byte[] buf = "AXAXAXAX".getBytes("UTF-8");

    // Act
    generalPurposeBit.encode(buf, 2);

    // Assert
    assertArrayEquals(new byte[]{'A', 'X', '\b', 0, 'A', 'X', 'A', 'X'}, buf);
  }

  /**
   * Test {@link GeneralPurposeBit#encode(byte[], int)} with {@code byte[]}, {@code int}.
   * <ul>
   *   <li>Then {@code AXAXAXAX} Bytes is {@code UTF-8} is array of {@code byte} with {@code A} and {@code X}.</li>
   * </ul>
   * <p>
   * Method under test: {@link GeneralPurposeBit#encode(byte[], int)}
   */
  @Test
  public void testEncodeWithByteInt_thenAxaxaxaxBytesIsUtf8IsArrayOfByteWithAAndX()
      throws UnsupportedEncodingException {
    // Arrange
    GeneralPurposeBit generalPurposeBit = new GeneralPurposeBit();
    byte[] buf = "AXAXAXAX".getBytes("UTF-8");

    // Act
    generalPurposeBit.encode(buf, 2);

    // Assert
    assertArrayEquals(new byte[]{'A', 'X', 0, 0, 'A', 'X', 'A', 'X'}, buf);
  }

  /**
   * Test {@link GeneralPurposeBit#encode(byte[], int)} with {@code byte[]}, {@code int}.
   * <ul>
   *   <li>Then {@code AXAXAXAX} Bytes is {@code UTF-8} is AXA backspace AXAX Bytes is {@code UTF-8}.</li>
   * </ul>
   * <p>
   * Method under test: {@link GeneralPurposeBit#encode(byte[], int)}
   */
  @Test
  public void testEncodeWithByteInt_thenAxaxaxaxBytesIsUtf8IsAxaBackspaceAxaxBytesIsUtf8()
      throws UnsupportedEncodingException {
    // Arrange
    GeneralPurposeBit parseResult = GeneralPurposeBit.parse(new byte[]{'A', -1, 'A', -1, 'A', -1, 'A', -1}, 2);
    byte[] buf = "AXAXAXAX".getBytes("UTF-8");

    // Act
    parseResult.encode(buf, 2);

    // Assert
    assertArrayEquals("AXA\bAXAX".getBytes("UTF-8"), buf);
  }

  /**
   * Test {@link GeneralPurposeBit#encode()}.
   * <ul>
   *   <li>Given {@code A}.</li>
   *   <li>Then return array of {@code byte} with {@code A} and zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link GeneralPurposeBit#encode()}
   */
  @Test
  public void testEncode_givenA_thenReturnArrayOfByteWithAAndZero() {
    // Arrange, Act and Assert
    assertArrayEquals(new byte[]{'A', 0},
        GeneralPurposeBit.parse(new byte[]{'A', 2, 'A', 2, 'A', 2, 'A', 2}, 2).encode());
  }

  /**
   * Test {@link GeneralPurposeBit#encode()}.
   * <ul>
   *   <li>Given {@link GeneralPurposeBit} (default constructor).</li>
   *   <li>Then return array of {@code byte} with zero and zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link GeneralPurposeBit#encode()}
   */
  @Test
  public void testEncode_givenGeneralPurposeBit_thenReturnArrayOfByteWithZeroAndZero() {
    // Arrange, Act and Assert
    assertArrayEquals(new byte[]{0, 0}, (new GeneralPurposeBit()).encode());
  }

  /**
   * Test {@link GeneralPurposeBit#encode()}.
   * <ul>
   *   <li>Then return array of {@code byte} with backspace and zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link GeneralPurposeBit#encode()}
   */
  @Test
  public void testEncode_thenReturnArrayOfByteWithBackspaceAndZero() {
    // Arrange
    GeneralPurposeBit generalPurposeBit = new GeneralPurposeBit();
    generalPurposeBit.useDataDescriptor(true);

    // Act and Assert
    assertArrayEquals(new byte[]{'\b', 0}, generalPurposeBit.encode());
  }

  /**
   * Test {@link GeneralPurposeBit#encode()}.
   * <ul>
   *   <li>Then return array of {@code byte} with zero and backspace.</li>
   * </ul>
   * <p>
   * Method under test: {@link GeneralPurposeBit#encode()}
   */
  @Test
  public void testEncode_thenReturnArrayOfByteWithZeroAndBackspace() {
    // Arrange
    GeneralPurposeBit generalPurposeBit = new GeneralPurposeBit();
    generalPurposeBit.useUTF8ForNames(true);

    // Act and Assert
    assertArrayEquals(new byte[]{0, '\b'}, generalPurposeBit.encode());
  }

  /**
   * Test {@link GeneralPurposeBit#parse(byte[], int)}.
   * <ul>
   *   <li>When AX backspace XAXAX Bytes is {@code UTF-8}.</li>
   *   <li>Then return not usesEncryption.</li>
   * </ul>
   * <p>
   * Method under test: {@link GeneralPurposeBit#parse(byte[], int)}
   */
  @Test
  public void testParse_whenAxBackspaceXaxaxBytesIsUtf8_thenReturnNotUsesEncryption()
      throws UnsupportedEncodingException {
    // Arrange and Act
    GeneralPurposeBit actualParseResult = GeneralPurposeBit.parse("AX\bXAXAX".getBytes("UTF-8"), 2);

    // Assert
    Object cloneResult = actualParseResult.clone();
    assertTrue(cloneResult instanceof GeneralPurposeBit);
    assertFalse(actualParseResult.usesEncryption());
    assertTrue(actualParseResult.usesDataDescriptor());
    assertEquals(actualParseResult, cloneResult);
    assertArrayEquals(new byte[]{'\b', '\b'}, actualParseResult.encode());
  }

  /**
   * Test {@link GeneralPurposeBit#parse(byte[], int)}.
   * <ul>
   *   <li>When {@code AXAAAXAX} Bytes is {@code UTF-8}.</li>
   *   <li>Then return encode is array of {@code byte} with {@code A} and zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link GeneralPurposeBit#parse(byte[], int)}
   */
  @Test
  public void testParse_whenAxaaaxaxBytesIsUtf8_thenReturnEncodeIsArrayOfByteWithAAndZero()
      throws UnsupportedEncodingException {
    // Arrange and Act
    GeneralPurposeBit actualParseResult = GeneralPurposeBit.parse("AXAAAXAX".getBytes("UTF-8"), 2);

    // Assert
    Object cloneResult = actualParseResult.clone();
    assertTrue(cloneResult instanceof GeneralPurposeBit);
    assertFalse(actualParseResult.usesDataDescriptor());
    assertTrue(actualParseResult.usesEncryption());
    assertEquals(actualParseResult, cloneResult);
    assertArrayEquals(new byte[]{'A', 0}, actualParseResult.encode());
  }

  /**
   * Test {@link GeneralPurposeBit#parse(byte[], int)}.
   * <ul>
   *   <li>When {@code AXAXAXAX} Bytes is {@code UTF-8}.</li>
   *   <li>Then return encode is array of {@code byte} with {@code A} and backspace.</li>
   * </ul>
   * <p>
   * Method under test: {@link GeneralPurposeBit#parse(byte[], int)}
   */
  @Test
  public void testParse_whenAxaxaxaxBytesIsUtf8_thenReturnEncodeIsArrayOfByteWithAAndBackspace()
      throws UnsupportedEncodingException {
    // Arrange and Act
    GeneralPurposeBit actualParseResult = GeneralPurposeBit.parse("AXAXAXAX".getBytes("UTF-8"), 2);

    // Assert
    Object cloneResult = actualParseResult.clone();
    assertTrue(cloneResult instanceof GeneralPurposeBit);
    assertFalse(actualParseResult.usesDataDescriptor());
    assertTrue(actualParseResult.usesEncryption());
    assertEquals(actualParseResult, cloneResult);
    assertArrayEquals(new byte[]{'A', '\b'}, actualParseResult.encode());
  }

  /**
   * Test {@link GeneralPurposeBit#parse(byte[], int)}.
   * <ul>
   *   <li>When {@code X}.</li>
   *   <li>Then return encode is array of {@code byte} with one and backspace.</li>
   * </ul>
   * <p>
   * Method under test: {@link GeneralPurposeBit#parse(byte[], int)}
   */
  @Test
  public void testParse_whenX_thenReturnEncodeIsArrayOfByteWithOneAndBackspace() {
    // Arrange and Act
    GeneralPurposeBit actualParseResult = GeneralPurposeBit.parse(new byte[]{'A', 'X', 1, 'X', 'A', 'X', 'A', 'X'}, 2);

    // Assert
    Object cloneResult = actualParseResult.clone();
    assertTrue(cloneResult instanceof GeneralPurposeBit);
    assertFalse(actualParseResult.usesDataDescriptor());
    assertTrue(actualParseResult.usesEncryption());
    assertEquals(actualParseResult, cloneResult);
    assertArrayEquals(new byte[]{1, '\b'}, actualParseResult.encode());
  }

  /**
   * Test {@link GeneralPurposeBit#equals(Object)}, and {@link GeneralPurposeBit#hashCode()}.
   * <ul>
   *   <li>When other is equal.</li>
   *   <li>Then return equal.</li>
   * </ul>
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link GeneralPurposeBit#equals(Object)}
   *   <li>{@link GeneralPurposeBit#hashCode()}
   * </ul>
   */
  @Test
  public void testEqualsAndHashCode_whenOtherIsEqual_thenReturnEqual() {
    // Arrange
    GeneralPurposeBit generalPurposeBit = new GeneralPurposeBit();
    GeneralPurposeBit generalPurposeBit2 = new GeneralPurposeBit();

    // Act and Assert
    assertEquals(generalPurposeBit, generalPurposeBit2);
    int expectedHashCodeResult = generalPurposeBit.hashCode();
    assertEquals(expectedHashCodeResult, generalPurposeBit2.hashCode());
  }

  /**
   * Test {@link GeneralPurposeBit#equals(Object)}, and {@link GeneralPurposeBit#hashCode()}.
   * <ul>
   *   <li>When other is same.</li>
   *   <li>Then return equal.</li>
   * </ul>
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link GeneralPurposeBit#equals(Object)}
   *   <li>{@link GeneralPurposeBit#hashCode()}
   * </ul>
   */
  @Test
  public void testEqualsAndHashCode_whenOtherIsSame_thenReturnEqual() {
    // Arrange
    GeneralPurposeBit generalPurposeBit = new GeneralPurposeBit();

    // Act and Assert
    assertEquals(generalPurposeBit, generalPurposeBit);
    int expectedHashCodeResult = generalPurposeBit.hashCode();
    assertEquals(expectedHashCodeResult, generalPurposeBit.hashCode());
  }

  /**
   * Test {@link GeneralPurposeBit#equals(Object)}.
   * <ul>
   *   <li>When other is different.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link GeneralPurposeBit#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsDifferent_thenReturnNotEqual() throws UnsupportedEncodingException {
    // Arrange
    GeneralPurposeBit parseResult = GeneralPurposeBit.parse("AXAXAXAX".getBytes("UTF-8"), 2);

    // Act and Assert
    assertNotEquals(parseResult, new GeneralPurposeBit());
  }

  /**
   * Test {@link GeneralPurposeBit#equals(Object)}.
   * <ul>
   *   <li>When other is different.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link GeneralPurposeBit#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsDifferent_thenReturnNotEqual2() {
    // Arrange
    GeneralPurposeBit generalPurposeBit = new GeneralPurposeBit();
    generalPurposeBit.useUTF8ForNames(true);

    // Act and Assert
    assertNotEquals(generalPurposeBit, new GeneralPurposeBit());
  }

  /**
   * Test {@link GeneralPurposeBit#equals(Object)}.
   * <ul>
   *   <li>When other is different.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link GeneralPurposeBit#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsDifferent_thenReturnNotEqual3() {
    // Arrange
    GeneralPurposeBit generalPurposeBit = new GeneralPurposeBit();
    generalPurposeBit.useDataDescriptor(true);

    // Act and Assert
    assertNotEquals(generalPurposeBit, new GeneralPurposeBit());
  }

  /**
   * Test {@link GeneralPurposeBit#equals(Object)}.
   * <ul>
   *   <li>When other is different.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link GeneralPurposeBit#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsDifferent_thenReturnNotEqual4() throws UnsupportedEncodingException {
    // Arrange
    GeneralPurposeBit parseResult = GeneralPurposeBit.parse("AXXXAXAX".getBytes("UTF-8"), 2);

    // Act and Assert
    assertNotEquals(parseResult, new GeneralPurposeBit());
  }

  /**
   * Test {@link GeneralPurposeBit#equals(Object)}.
   * <ul>
   *   <li>When other is {@code null}.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link GeneralPurposeBit#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsNull_thenReturnNotEqual() {
    // Arrange, Act and Assert
    assertNotEquals(new GeneralPurposeBit(), null);
  }

  /**
   * Test {@link GeneralPurposeBit#equals(Object)}.
   * <ul>
   *   <li>When other is wrong type.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link GeneralPurposeBit#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsWrongType_thenReturnNotEqual() {
    // Arrange, Act and Assert
    assertNotEquals(new GeneralPurposeBit(), "Different type to GeneralPurposeBit");
  }

  /**
   * Test {@link GeneralPurposeBit#clone()}.
   * <p>
   * Method under test: {@link GeneralPurposeBit#clone()}
   */
  @Test
  public void testClone() {
    // Arrange
    GeneralPurposeBit generalPurposeBit = new GeneralPurposeBit();

    // Act
    Object actualCloneResult = generalPurposeBit.clone();

    // Assert
    assertTrue(actualCloneResult instanceof GeneralPurposeBit);
    assertEquals(generalPurposeBit, actualCloneResult);
  }
}
