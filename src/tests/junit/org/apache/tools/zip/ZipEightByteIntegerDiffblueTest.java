package org.apache.tools.zip;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertSame;
import java.io.UnsupportedEncodingException;
import java.math.BigInteger;
import org.junit.Test;

public class ZipEightByteIntegerDiffblueTest {
  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link ZipEightByteInteger#ZipEightByteInteger(BigInteger)}
   *   <li>{@link ZipEightByteInteger#toString()}
   *   <li>{@link ZipEightByteInteger#getValue()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    ZipEightByteInteger actualZipEightByteInteger = new ZipEightByteInteger(BigInteger.valueOf(1L));
    String actualToStringResult = actualZipEightByteInteger.toString();
    BigInteger actualValue = actualZipEightByteInteger.getValue();

    // Assert
    assertEquals("ZipEightByteInteger value: 1", actualToStringResult);
    assertSame(actualValue.ONE, actualValue);
  }

  /**
   * Test {@link ZipEightByteInteger#ZipEightByteInteger(long)}.
   * <p>
   * Method under test: {@link ZipEightByteInteger#ZipEightByteInteger(long)}
   */
  @Test
  public void testNewZipEightByteInteger() {
    // Arrange and Act
    ZipEightByteInteger actualZipEightByteInteger = new ZipEightByteInteger(42L);

    // Assert
    BigInteger value = actualZipEightByteInteger.getValue();
    assertEquals("42", value.toString());
    assertEquals(1, value.getLowestSetBit());
    assertEquals(1, value.signum());
    assertEquals(42L, actualZipEightByteInteger.getLongValue());
    assertArrayEquals(new byte[]{'*'}, value.toByteArray());
    assertArrayEquals(new byte[]{'*', 0, 0, 0, 0, 0, 0, 0}, actualZipEightByteInteger.getBytes());
  }

  /**
   * Test {@link ZipEightByteInteger#ZipEightByteInteger(byte[])}.
   * <ul>
   *   <li>Then return Value toString is {@code 6359461186500646977}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEightByteInteger#ZipEightByteInteger(byte[])}
   */
  @Test
  public void testNewZipEightByteInteger_thenReturnValueToStringIs6359461186500646977()
      throws UnsupportedEncodingException {
    // Arrange and Act
    ZipEightByteInteger actualZipEightByteInteger = new ZipEightByteInteger("AXAXAXAX".getBytes("UTF-8"));

    // Assert
    BigInteger value = actualZipEightByteInteger.getValue();
    assertEquals("6359461186500646977", value.toString());
    assertEquals(6359461186500646977L, actualZipEightByteInteger.getLongValue());
    byte[] expectedBytes = "AXAXAXAX".getBytes("UTF-8");
    assertArrayEquals(expectedBytes, actualZipEightByteInteger.getBytes());
    byte[] expectedToByteArrayResult = "XAXAXAXA".getBytes("UTF-8");
    assertArrayEquals(expectedToByteArrayResult, value.toByteArray());
  }

  /**
   * Test {@link ZipEightByteInteger#ZipEightByteInteger(byte[], int)}.
   * <ul>
   *   <li>When {@code A}.</li>
   *   <li>Then return Value toString is {@code 522707007627659073}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEightByteInteger#ZipEightByteInteger(byte[], int)}
   */
  @Test
  public void testNewZipEightByteInteger_whenA_thenReturnValueToStringIs522707007627659073() {
    // Arrange and Act
    ZipEightByteInteger actualZipEightByteInteger = new ZipEightByteInteger(
        new byte[]{'A', 7, 'A', 7, 'A', 7, 'A', 7, 'A', 7, 'A', 7, 'A', 7, 'A', 7}, 2);

    // Assert
    BigInteger value = actualZipEightByteInteger.getValue();
    assertEquals("522707007627659073", value.toString());
    assertEquals(522707007627659073L, actualZipEightByteInteger.getLongValue());
    assertArrayEquals(new byte[]{7, 'A', 7, 'A', 7, 'A', 7, 'A'}, value.toByteArray());
    assertArrayEquals(new byte[]{'A', 7, 'A', 7, 'A', 7, 'A', 7}, actualZipEightByteInteger.getBytes());
  }

  /**
   * Test {@link ZipEightByteInteger#ZipEightByteInteger(byte[], int)}.
   * <ul>
   *   <li>When {@code A}.</li>
   *   <li>Then return Value toString is {@code 9241675886216939329}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEightByteInteger#ZipEightByteInteger(byte[], int)}
   */
  @Test
  public void testNewZipEightByteInteger_whenA_thenReturnValueToStringIs9241675886216939329() {
    // Arrange and Act
    ZipEightByteInteger actualZipEightByteInteger = new ZipEightByteInteger(
        new byte[]{'A', 7, 'A', 7, 'A', 7, 'A', 7, 'A', Byte.MIN_VALUE, 'A', 7, 'A', 7, 'A', 7}, 2);

    // Assert
    BigInteger value = actualZipEightByteInteger.getValue();
    assertEquals("9241675886216939329", value.toString());
    assertEquals(-9205068187492612287L, actualZipEightByteInteger.getLongValue());
    assertArrayEquals(new byte[]{'A', 7, 'A', 7, 'A', 7, 'A', Byte.MIN_VALUE}, actualZipEightByteInteger.getBytes());
    assertArrayEquals(new byte[]{0, Byte.MIN_VALUE, 'A', 7, 'A', 7, 'A', 7, 'A'}, value.toByteArray());
  }

  /**
   * Test {@link ZipEightByteInteger#ZipEightByteInteger(byte[])}.
   * <ul>
   *   <li>When {@code A}.</li>
   *   <li>Then return Value toString is {@code 9241764948017764417}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEightByteInteger#ZipEightByteInteger(byte[])}
   */
  @Test
  public void testNewZipEightByteInteger_whenA_thenReturnValueToStringIs9241764948017764417() {
    // Arrange and Act
    ZipEightByteInteger actualZipEightByteInteger = new ZipEightByteInteger(
        new byte[]{'A', 'X', 'A', 'X', 'A', 'X', 'A', Byte.MIN_VALUE});

    // Assert
    BigInteger value = actualZipEightByteInteger.getValue();
    assertEquals("9241764948017764417", value.toString());
    assertEquals(-9204979125691787199L, actualZipEightByteInteger.getLongValue());
    assertArrayEquals(new byte[]{'A', 'X', 'A', 'X', 'A', 'X', 'A', Byte.MIN_VALUE},
        actualZipEightByteInteger.getBytes());
    assertArrayEquals(new byte[]{0, Byte.MIN_VALUE, 'A', 'X', 'A', 'X', 'A', 'X', 'A'}, value.toByteArray());
  }

  /**
   * Test {@link ZipEightByteInteger#getBytes(BigInteger)} with {@code BigInteger}.
   * <ul>
   *   <li>Then return array of {@code byte} with minus one and minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEightByteInteger#getBytes(BigInteger)}
   */
  @Test
  public void testGetBytesWithBigInteger_thenReturnArrayOfByteWithMinusOneAndMinusOne() {
    // Arrange, Act and Assert
    assertArrayEquals(new byte[]{-1, -1, -1, -1, -1, -1, -1, -1},
        ZipEightByteInteger.getBytes(BigInteger.valueOf(-1L)));
  }

  /**
   * Test {@link ZipEightByteInteger#getBytes(BigInteger)} with {@code BigInteger}.
   * <ul>
   *   <li>When valueOf one.</li>
   *   <li>Then return array of {@code byte} with one and zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEightByteInteger#getBytes(BigInteger)}
   */
  @Test
  public void testGetBytesWithBigInteger_whenValueOfOne_thenReturnArrayOfByteWithOneAndZero() {
    // Arrange, Act and Assert
    assertArrayEquals(new byte[]{1, 0, 0, 0, 0, 0, 0, 0}, ZipEightByteInteger.getBytes(BigInteger.valueOf(1L)));
  }

  /**
   * Test {@link ZipEightByteInteger#getBytes(long)} with {@code long}.
   * <ul>
   *   <li>When forty-two.</li>
   *   <li>Then return array of {@code byte} with {@code *} and zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEightByteInteger#getBytes(long)}
   */
  @Test
  public void testGetBytesWithLong_whenFortyTwo_thenReturnArrayOfByteWithAsteriskAndZero() {
    // Arrange, Act and Assert
    assertArrayEquals(new byte[]{'*', 0, 0, 0, 0, 0, 0, 0}, ZipEightByteInteger.getBytes(42L));
  }

  /**
   * Test {@link ZipEightByteInteger#getBytes(long)} with {@code long}.
   * <ul>
   *   <li>When minus one.</li>
   *   <li>Then return array of {@code byte} with minus one and minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEightByteInteger#getBytes(long)}
   */
  @Test
  public void testGetBytesWithLong_whenMinusOne_thenReturnArrayOfByteWithMinusOneAndMinusOne() {
    // Arrange, Act and Assert
    assertArrayEquals(new byte[]{-1, -1, -1, -1, -1, -1, -1, -1}, ZipEightByteInteger.getBytes(-1L));
  }

  /**
   * Test {@link ZipEightByteInteger#getBytes()}.
   * <ul>
   *   <li>Given {@link ZipEightByteInteger#ZERO}.</li>
   *   <li>Then return array of {@code byte} with zero and zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEightByteInteger#getBytes()}
   */
  @Test
  public void testGetBytes_givenZero_thenReturnArrayOfByteWithZeroAndZero() {
    // Arrange, Act and Assert
    assertArrayEquals(new byte[]{0, 0, 0, 0, 0, 0, 0, 0}, ZipEightByteInteger.ZERO.getBytes());
  }

  /**
   * Test {@link ZipEightByteInteger#getBytes()}.
   * <ul>
   *   <li>Then return array of {@code byte} with minus one and minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEightByteInteger#getBytes()}
   */
  @Test
  public void testGetBytes_thenReturnArrayOfByteWithMinusOneAndMinusOne() {
    // Arrange, Act and Assert
    assertArrayEquals(new byte[]{-1, -1, -1, -1, -1, -1, -1, -1}, (new ZipEightByteInteger(-1L)).getBytes());
  }

  /**
   * Test {@link ZipEightByteInteger#getLongValue(byte[], int)} with {@code bytes}, {@code offset}.
   * <ul>
   *   <li>When {@code A}.</li>
   *   <li>Then return {@code 522707007627659073}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEightByteInteger#getLongValue(byte[], int)}
   */
  @Test
  public void testGetLongValueWithBytesOffset_whenA_thenReturn522707007627659073() {
    // Arrange, Act and Assert
    assertEquals(522707007627659073L, ZipEightByteInteger
        .getLongValue(new byte[]{'A', 7, 'A', 7, 'A', 7, 'A', 7, 'A', 7, 'A', 7, 'A', 7, 'A', 7}, 2));
  }

  /**
   * Test {@link ZipEightByteInteger#getLongValue(byte[], int)} with {@code bytes}, {@code offset}.
   * <ul>
   *   <li>When {@code A}.</li>
   *   <li>Then return {@code -9205068187492612287}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEightByteInteger#getLongValue(byte[], int)}
   */
  @Test
  public void testGetLongValueWithBytesOffset_whenA_thenReturn9205068187492612287() {
    // Arrange, Act and Assert
    assertEquals(-9205068187492612287L, ZipEightByteInteger
        .getLongValue(new byte[]{'A', 7, 'A', 7, 'A', 7, 'A', 7, 'A', Byte.MIN_VALUE, 'A', 7, 'A', 7, 'A', 7}, 2));
  }

  /**
   * Test {@link ZipEightByteInteger#getLongValue(byte[])} with {@code bytes}.
   * <ul>
   *   <li>When {@code A}.</li>
   *   <li>Then return {@code -9204979125691787199}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEightByteInteger#getLongValue(byte[])}
   */
  @Test
  public void testGetLongValueWithBytes_whenA_thenReturn9204979125691787199() {
    // Arrange, Act and Assert
    assertEquals(-9204979125691787199L,
        ZipEightByteInteger.getLongValue(new byte[]{'A', 'X', 'A', 'X', 'A', 'X', 'A', Byte.MIN_VALUE}));
  }

  /**
   * Test {@link ZipEightByteInteger#getLongValue(byte[])} with {@code bytes}.
   * <ul>
   *   <li>When {@code AXAXAXAX} Bytes is {@code UTF-8}.</li>
   *   <li>Then return {@code 6359461186500646977}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEightByteInteger#getLongValue(byte[])}
   */
  @Test
  public void testGetLongValueWithBytes_whenAxaxaxaxBytesIsUtf8_thenReturn6359461186500646977()
      throws UnsupportedEncodingException {
    // Arrange, Act and Assert
    assertEquals(6359461186500646977L, ZipEightByteInteger.getLongValue("AXAXAXAX".getBytes("UTF-8")));
  }

  /**
   * Test {@link ZipEightByteInteger#getLongValue()}.
   * <ul>
   *   <li>Given {@link ZipEightByteInteger#ZERO}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEightByteInteger#getLongValue()}
   */
  @Test
  public void testGetLongValue_givenZero_thenReturnZero() {
    // Arrange, Act and Assert
    assertEquals(0L, ZipEightByteInteger.ZERO.getLongValue());
  }

  /**
   * Test {@link ZipEightByteInteger#getValue(byte[], int)} with {@code bytes}, {@code offset}.
   * <ul>
   *   <li>When {@code A}.</li>
   *   <li>Then return toString is {@code 522707007627659073}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEightByteInteger#getValue(byte[], int)}
   */
  @Test
  public void testGetValueWithBytesOffset_whenA_thenReturnToStringIs522707007627659073() {
    // Arrange and Act
    BigInteger actualValue = ZipEightByteInteger
        .getValue(new byte[]{'A', 7, 'A', 7, 'A', 7, 'A', 7, 'A', 7, 'A', 7, 'A', 7, 'A', 7}, 2);

    // Assert
    assertEquals("522707007627659073", actualValue.toString());
    assertEquals(0, actualValue.getLowestSetBit());
    assertEquals(1, actualValue.signum());
    assertArrayEquals(new byte[]{7, 'A', 7, 'A', 7, 'A', 7, 'A'}, actualValue.toByteArray());
  }

  /**
   * Test {@link ZipEightByteInteger#getValue(byte[], int)} with {@code bytes}, {@code offset}.
   * <ul>
   *   <li>When {@code A}.</li>
   *   <li>Then return toString is {@code 9241675886216939329}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEightByteInteger#getValue(byte[], int)}
   */
  @Test
  public void testGetValueWithBytesOffset_whenA_thenReturnToStringIs9241675886216939329() {
    // Arrange and Act
    BigInteger actualValue = ZipEightByteInteger
        .getValue(new byte[]{'A', 7, 'A', 7, 'A', 7, 'A', 7, 'A', Byte.MIN_VALUE, 'A', 7, 'A', 7, 'A', 7}, 2);

    // Assert
    assertEquals("9241675886216939329", actualValue.toString());
    assertEquals(0, actualValue.getLowestSetBit());
    assertEquals(1, actualValue.signum());
    assertArrayEquals(new byte[]{0, Byte.MIN_VALUE, 'A', 7, 'A', 7, 'A', 7, 'A'}, actualValue.toByteArray());
  }

  /**
   * Test {@link ZipEightByteInteger#getValue(byte[])} with {@code bytes}.
   * <ul>
   *   <li>Then return toString is {@code 6359461186500646977}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEightByteInteger#getValue(byte[])}
   */
  @Test
  public void testGetValueWithBytes_thenReturnToStringIs6359461186500646977() throws UnsupportedEncodingException {
    // Arrange and Act
    BigInteger actualValue = ZipEightByteInteger.getValue("AXAXAXAX".getBytes("UTF-8"));

    // Assert
    assertEquals("6359461186500646977", actualValue.toString());
    assertEquals(0, actualValue.getLowestSetBit());
    assertEquals(1, actualValue.signum());
    byte[] expectedToByteArrayResult = "XAXAXAXA".getBytes("UTF-8");
    assertArrayEquals(expectedToByteArrayResult, actualValue.toByteArray());
  }

  /**
   * Test {@link ZipEightByteInteger#getValue(byte[])} with {@code bytes}.
   * <ul>
   *   <li>When {@code A}.</li>
   *   <li>Then return toString is {@code 9241764948017764417}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEightByteInteger#getValue(byte[])}
   */
  @Test
  public void testGetValueWithBytes_whenA_thenReturnToStringIs9241764948017764417() {
    // Arrange and Act
    BigInteger actualValue = ZipEightByteInteger
        .getValue(new byte[]{'A', 'X', 'A', 'X', 'A', 'X', 'A', Byte.MIN_VALUE});

    // Assert
    assertEquals("9241764948017764417", actualValue.toString());
    assertEquals(0, actualValue.getLowestSetBit());
    assertEquals(1, actualValue.signum());
    assertArrayEquals(new byte[]{0, Byte.MIN_VALUE, 'A', 'X', 'A', 'X', 'A', 'X', 'A'}, actualValue.toByteArray());
  }

  /**
   * Test {@link ZipEightByteInteger#equals(Object)}, and {@link ZipEightByteInteger#hashCode()}.
   * <ul>
   *   <li>When other is equal.</li>
   *   <li>Then return equal.</li>
   * </ul>
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link ZipEightByteInteger#equals(Object)}
   *   <li>{@link ZipEightByteInteger#hashCode()}
   * </ul>
   */
  @Test
  public void testEqualsAndHashCode_whenOtherIsEqual_thenReturnEqual() {
    // Arrange
    ZipEightByteInteger zipEightByteInteger = ZipEightByteInteger.ZERO;
    ZipEightByteInteger zipEightByteInteger2 = ZipEightByteInteger.ZERO;

    // Act and Assert
    assertEquals(zipEightByteInteger, zipEightByteInteger2);
    int expectedHashCodeResult = zipEightByteInteger.hashCode();
    assertEquals(expectedHashCodeResult, zipEightByteInteger2.hashCode());
  }

  /**
   * Test {@link ZipEightByteInteger#equals(Object)}, and {@link ZipEightByteInteger#hashCode()}.
   * <ul>
   *   <li>When other is same.</li>
   *   <li>Then return equal.</li>
   * </ul>
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link ZipEightByteInteger#equals(Object)}
   *   <li>{@link ZipEightByteInteger#hashCode()}
   * </ul>
   */
  @Test
  public void testEqualsAndHashCode_whenOtherIsSame_thenReturnEqual() {
    // Arrange
    ZipEightByteInteger zipEightByteInteger = ZipEightByteInteger.ZERO;

    // Act and Assert
    assertEquals(zipEightByteInteger, zipEightByteInteger);
    int expectedHashCodeResult = zipEightByteInteger.hashCode();
    assertEquals(expectedHashCodeResult, zipEightByteInteger.hashCode());
  }

  /**
   * Test {@link ZipEightByteInteger#equals(Object)}.
   * <ul>
   *   <li>When other is different.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEightByteInteger#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsDifferent_thenReturnNotEqual() {
    // Arrange, Act and Assert
    assertNotEquals(new ZipEightByteInteger(42L), ZipEightByteInteger.ZERO);
  }

  /**
   * Test {@link ZipEightByteInteger#equals(Object)}.
   * <ul>
   *   <li>When other is {@code null}.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEightByteInteger#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsNull_thenReturnNotEqual() {
    // Arrange, Act and Assert
    assertNotEquals(ZipEightByteInteger.ZERO, null);
  }

  /**
   * Test {@link ZipEightByteInteger#equals(Object)}.
   * <ul>
   *   <li>When other is wrong type.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEightByteInteger#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsWrongType_thenReturnNotEqual() {
    // Arrange, Act and Assert
    assertNotEquals(ZipEightByteInteger.ZERO, "Different type to ZipEightByteInteger");
  }
}
