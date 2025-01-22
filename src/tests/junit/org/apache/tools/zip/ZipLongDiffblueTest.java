package org.apache.tools.zip;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertTrue;
import java.io.UnsupportedEncodingException;
import org.junit.Test;

public class ZipLongDiffblueTest {
  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link ZipLong#ZipLong(long)}
   *   <li>{@link ZipLong#toString()}
   *   <li>{@link ZipLong#getValue()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    ZipLong actualZipLong = new ZipLong(42L);
    String actualToStringResult = actualZipLong.toString();

    // Assert
    assertEquals("ZipLong value: 42", actualToStringResult);
    assertEquals(42L, actualZipLong.getValue());
  }

  /**
   * Test {@link ZipLong#ZipLong(byte[])}.
   * <ul>
   *   <li>When {@code AXAXAXAX} Bytes is {@code UTF-8}.</li>
   *   <li>Then return Value is {@code 1480677441}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipLong#ZipLong(byte[])}
   */
  @Test
  public void testNewZipLong_whenAxaxaxaxBytesIsUtf8_thenReturnValueIs1480677441() throws UnsupportedEncodingException {
    // Arrange and Act
    ZipLong actualZipLong = new ZipLong("AXAXAXAX".getBytes("UTF-8"));

    // Assert
    assertEquals(1480677441L, actualZipLong.getValue());
    byte[] expectedBytes = "AXAX".getBytes("UTF-8");
    assertArrayEquals(expectedBytes, actualZipLong.getBytes());
  }

  /**
   * Test {@link ZipLong#ZipLong(byte[], int)}.
   * <ul>
   *   <li>When {@code AXAXAXAX} Bytes is {@code UTF-8}.</li>
   *   <li>Then return Value is {@code 1480677441}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipLong#ZipLong(byte[], int)}
   */
  @Test
  public void testNewZipLong_whenAxaxaxaxBytesIsUtf8_thenReturnValueIs14806774412()
      throws UnsupportedEncodingException {
    // Arrange and Act
    ZipLong actualZipLong = new ZipLong("AXAXAXAX".getBytes("UTF-8"), 2);

    // Assert
    assertEquals(1480677441L, actualZipLong.getValue());
    byte[] expectedBytes = "AXAX".getBytes("UTF-8");
    assertArrayEquals(expectedBytes, actualZipLong.getBytes());
  }

  /**
   * Test {@link ZipLong#getBytes()}.
   * <p>
   * Method under test: {@link ZipLong#getBytes()}
   */
  @Test
  public void testGetBytes() {
    // Arrange, Act and Assert
    assertArrayEquals(new byte[]{'P', 'K', 1, 2}, ZipLong.CFH_SIG.getBytes());
  }

  /**
   * Test {@link ZipLong#getBytes(long)} with {@code long}.
   * <p>
   * Method under test: {@link ZipLong#getBytes(long)}
   */
  @Test
  public void testGetBytesWithLong() {
    // Arrange, Act and Assert
    assertArrayEquals(new byte[]{'*', 0, 0, 0}, ZipLong.getBytes(42L));
  }

  /**
   * Test {@link ZipLong#putLong(byte[], int)} with {@code buf}, {@code offset}.
   * <ul>
   *   <li>Then {@code AXAXAXAX} Bytes is {@code UTF-8} is array of {@code byte} with {@code A} and {@code X}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipLong#putLong(byte[], int)}
   */
  @Test
  public void testPutLongWithBufOffset_thenAxaxaxaxBytesIsUtf8IsArrayOfByteWithAAndX()
      throws UnsupportedEncodingException {
    // Arrange
    byte[] buf = "AXAXAXAX".getBytes("UTF-8");

    // Act
    ZipLong.CFH_SIG.putLong(buf, 2);

    // Assert
    assertArrayEquals(new byte[]{'A', 'X', 'P', 'K', 1, 2, 'A', 'X'}, buf);
  }

  /**
   * Test {@link ZipLong#putLong(long, byte[], int)} with {@code value}, {@code buf}, {@code offset}.
   * <ul>
   *   <li>Then {@code AXAXAXAX} Bytes is {@code UTF-8} is array of {@code byte} with {@code A} and {@code X}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipLong#putLong(long, byte[], int)}
   */
  @Test
  public void testPutLongWithValueBufOffset_thenAxaxaxaxBytesIsUtf8IsArrayOfByteWithAAndX()
      throws UnsupportedEncodingException {
    // Arrange
    byte[] buf = "AXAXAXAX".getBytes("UTF-8");

    // Act
    ZipLong.putLong(42L, buf, 2);

    // Assert
    assertArrayEquals(new byte[]{'A', 'X', '*', 0, 0, 0, 'A', 'X'}, buf);
  }

  /**
   * Test {@link ZipLong#getValue(byte[], int)} with {@code bytes}, {@code offset}.
   * <ul>
   *   <li>When {@code AXAXAXAX} Bytes is {@code UTF-8}.</li>
   *   <li>Then return {@code 1480677441}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipLong#getValue(byte[], int)}
   */
  @Test
  public void testGetValueWithBytesOffset_whenAxaxaxaxBytesIsUtf8_thenReturn1480677441()
      throws UnsupportedEncodingException {
    // Arrange, Act and Assert
    assertEquals(1480677441L, ZipLong.getValue("AXAXAXAX".getBytes("UTF-8"), 2));
  }

  /**
   * Test {@link ZipLong#getValue(byte[])} with {@code bytes}.
   * <ul>
   *   <li>When {@code AXAXAXAX} Bytes is {@code UTF-8}.</li>
   *   <li>Then return {@code 1480677441}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipLong#getValue(byte[])}
   */
  @Test
  public void testGetValueWithBytes_whenAxaxaxaxBytesIsUtf8_thenReturn1480677441() throws UnsupportedEncodingException {
    // Arrange, Act and Assert
    assertEquals(1480677441L, ZipLong.getValue("AXAXAXAX".getBytes("UTF-8")));
  }

  /**
   * Test {@link ZipLong#equals(Object)}, and {@link ZipLong#hashCode()}.
   * <ul>
   *   <li>When other is equal.</li>
   *   <li>Then return equal.</li>
   * </ul>
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link ZipLong#equals(Object)}
   *   <li>{@link ZipLong#hashCode()}
   * </ul>
   */
  @Test
  public void testEqualsAndHashCode_whenOtherIsEqual_thenReturnEqual() {
    // Arrange
    ZipLong zipLong = ZipLong.CFH_SIG;
    ZipLong zipLong2 = ZipLong.CFH_SIG;

    // Act and Assert
    assertEquals(zipLong, zipLong2);
    int expectedHashCodeResult = zipLong.hashCode();
    assertEquals(expectedHashCodeResult, zipLong2.hashCode());
  }

  /**
   * Test {@link ZipLong#equals(Object)}, and {@link ZipLong#hashCode()}.
   * <ul>
   *   <li>When other is same.</li>
   *   <li>Then return equal.</li>
   * </ul>
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link ZipLong#equals(Object)}
   *   <li>{@link ZipLong#hashCode()}
   * </ul>
   */
  @Test
  public void testEqualsAndHashCode_whenOtherIsSame_thenReturnEqual() {
    // Arrange
    ZipLong zipLong = ZipLong.CFH_SIG;

    // Act and Assert
    assertEquals(zipLong, zipLong);
    int expectedHashCodeResult = zipLong.hashCode();
    assertEquals(expectedHashCodeResult, zipLong.hashCode());
  }

  /**
   * Test {@link ZipLong#equals(Object)}.
   * <ul>
   *   <li>When other is different.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipLong#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsDifferent_thenReturnNotEqual() {
    // Arrange, Act and Assert
    assertNotEquals(ZipLong.DD_SIG, ZipLong.CFH_SIG);
  }

  /**
   * Test {@link ZipLong#equals(Object)}.
   * <ul>
   *   <li>When other is {@code null}.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipLong#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsNull_thenReturnNotEqual() {
    // Arrange, Act and Assert
    assertNotEquals(ZipLong.CFH_SIG, null);
  }

  /**
   * Test {@link ZipLong#equals(Object)}.
   * <ul>
   *   <li>When other is wrong type.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipLong#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsWrongType_thenReturnNotEqual() {
    // Arrange, Act and Assert
    assertNotEquals(ZipLong.CFH_SIG, "Different type to ZipLong");
  }

  /**
   * Test {@link ZipLong#clone()}.
   * <p>
   * Method under test: {@link ZipLong#clone()}
   */
  @Test
  public void testClone() {
    // Arrange, Act and Assert
    assertTrue(ZipLong.CFH_SIG.clone() instanceof ZipLong);
  }
}
