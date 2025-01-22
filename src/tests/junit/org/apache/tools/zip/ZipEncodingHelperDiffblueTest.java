package org.apache.tools.zip;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.nio.ByteBuffer;
import org.junit.Test;

public class ZipEncodingHelperDiffblueTest {
  /**
   * Test {@link ZipEncodingHelper#growBuffer(ByteBuffer, int)}.
   * <ul>
   *   <li>Then wrap {@code AXAXAXAX} Bytes is {@code UTF-8} limit is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEncodingHelper#growBuffer(ByteBuffer, int)}
   */
  @Test
  public void testGrowBuffer_thenWrapAxaxaxaxBytesIsUtf8LimitIsZero() throws UnsupportedEncodingException {
    // Arrange
    ByteBuffer b = ByteBuffer.wrap("AXAXAXAX".getBytes("UTF-8"));

    // Act
    ByteBuffer actualGrowBufferResult = ZipEncodingHelper.growBuffer(b, 1);

    // Assert
    assertEquals(0, b.limit());
    assertFalse(b.hasRemaining());
    assertEquals(Short.SIZE, actualGrowBufferResult.capacity());
    assertEquals(Short.SIZE, actualGrowBufferResult.limit());
    assertArrayEquals(new byte[]{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, actualGrowBufferResult.array());
  }

  /**
   * Test {@link ZipEncodingHelper#growBuffer(ByteBuffer, int)}.
   * <ul>
   *   <li>When wrap empty array of {@code byte}.</li>
   *   <li>Then wrap empty array of {@code byte} limit is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEncodingHelper#growBuffer(ByteBuffer, int)}
   */
  @Test
  public void testGrowBuffer_whenWrapEmptyArrayOfByte_thenWrapEmptyArrayOfByteLimitIsZero() {
    // Arrange
    ByteBuffer b = ByteBuffer.wrap(new byte[]{});

    // Act
    ByteBuffer actualGrowBufferResult = ZipEncodingHelper.growBuffer(b, 1);

    // Assert
    assertEquals(0, b.limit());
    assertEquals(1, actualGrowBufferResult.capacity());
    assertEquals(1, actualGrowBufferResult.limit());
    assertFalse(b.hasRemaining());
    assertArrayEquals(new byte[]{0}, actualGrowBufferResult.array());
  }

  /**
   * Test {@link ZipEncodingHelper#appendSurrogate(ByteBuffer, char)}.
   * <ul>
   *   <li>Then wrap {@code AXAXAXAX} Bytes is {@code UTF-8} position is six.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEncodingHelper#appendSurrogate(ByteBuffer, char)}
   */
  @Test
  public void testAppendSurrogate_thenWrapAxaxaxaxBytesIsUtf8PositionIsSix() throws UnsupportedEncodingException {
    // Arrange
    ByteBuffer bb = ByteBuffer.wrap("AXAXAXAX".getBytes("UTF-8"));

    // Act
    ZipEncodingHelper.appendSurrogate(bb, 'A');

    // Assert
    assertEquals(6, bb.position());
    byte[] expectedArrayResult = "%U0041AX".getBytes("UTF-8");
    assertArrayEquals(expectedArrayResult, bb.array());
  }

  /**
   * Test {@link ZipEncodingHelper#getZipEncoding(String)}.
   * <ul>
   *   <li>When {@code ibm437}.</li>
   *   <li>Then return {@link Simple8BitZipEncoding}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEncodingHelper#getZipEncoding(String)}
   */
  @Test
  public void testGetZipEncoding_whenIbm437_thenReturnSimple8BitZipEncoding() {
    // Arrange and Act
    ZipEncoding actualZipEncoding = ZipEncodingHelper.getZipEncoding("ibm437");

    // Assert
    assertTrue(actualZipEncoding instanceof Simple8BitZipEncoding);
    assertEquals('A', ((Simple8BitZipEncoding) actualZipEncoding).decodeByte((byte) 'A'));
    assertTrue(((Simple8BitZipEncoding) actualZipEncoding).canEncodeChar('A'));
    assertTrue(actualZipEncoding.canEncode("Name"));
  }

  /**
   * Test {@link ZipEncodingHelper#getZipEncoding(String)}.
   * <ul>
   *   <li>When {@code Name}.</li>
   *   <li>Then return canEncode {@code Name}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEncodingHelper#getZipEncoding(String)}
   */
  @Test
  public void testGetZipEncoding_whenName_thenReturnCanEncodeName() {
    // Arrange and Act
    ZipEncoding actualZipEncoding = ZipEncodingHelper.getZipEncoding("Name");

    // Assert
    assertTrue(actualZipEncoding instanceof FallbackZipEncoding);
    assertTrue(actualZipEncoding.canEncode("Name"));
  }

  /**
   * Test {@link ZipEncodingHelper#getZipEncoding(String)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return encode {@code Name} position is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEncodingHelper#getZipEncoding(String)}
   */
  @Test
  public void testGetZipEncoding_whenNull_thenReturnEncodeNamePositionIsZero() throws IOException {
    // Arrange and Act
    ZipEncoding actualZipEncoding = ZipEncodingHelper.getZipEncoding(null);

    // Assert
    assertTrue(actualZipEncoding instanceof FallbackZipEncoding);
    ByteBuffer encodeResult = actualZipEncoding.encode("Name");
    assertEquals(0, encodeResult.position());
    assertEquals(4, encodeResult.capacity());
    assertEquals(4, encodeResult.limit());
    assertTrue(encodeResult.hasRemaining());
    assertTrue(encodeResult.hasArray());
    byte[] expectedArrayResult = "Name".getBytes("UTF-8");
    assertArrayEquals(expectedArrayResult, encodeResult.array());
  }

  /**
   * Test {@link ZipEncodingHelper#getZipEncoding(String)}.
   * <ul>
   *   <li>When {@code UTF8}.</li>
   *   <li>Then return encode {@code Name} position is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEncodingHelper#getZipEncoding(String)}
   */
  @Test
  public void testGetZipEncoding_whenUtf8_thenReturnEncodeNamePositionIsZero() throws IOException {
    // Arrange and Act
    ZipEncoding actualZipEncoding = ZipEncodingHelper.getZipEncoding("UTF8");

    // Assert
    assertTrue(actualZipEncoding instanceof FallbackZipEncoding);
    ByteBuffer encodeResult = actualZipEncoding.encode("Name");
    assertEquals(0, encodeResult.position());
    assertEquals(4, encodeResult.capacity());
    assertEquals(4, encodeResult.limit());
    assertTrue(encodeResult.hasRemaining());
    assertTrue(encodeResult.hasArray());
    byte[] expectedArrayResult = "Name".getBytes("UTF-8");
    assertArrayEquals(expectedArrayResult, encodeResult.array());
  }

  /**
   * Test {@link ZipEncodingHelper#isUTF8(String)}.
   * <ul>
   *   <li>When {@code Encoding}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEncodingHelper#isUTF8(String)}
   */
  @Test
  public void testIsUTF8_whenEncoding_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(ZipEncodingHelper.isUTF8("Encoding"));
  }

  /**
   * Test {@link ZipEncodingHelper#isUTF8(String)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEncodingHelper#isUTF8(String)}
   */
  @Test
  public void testIsUTF8_whenNull_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(ZipEncodingHelper.isUTF8(null));
  }

  /**
   * Test {@link ZipEncodingHelper#isUTF8(String)}.
   * <ul>
   *   <li>When {@code UTF-8}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEncodingHelper#isUTF8(String)}
   */
  @Test
  public void testIsUTF8_whenUtf8_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(ZipEncodingHelper.isUTF8("UTF-8"));
  }

  /**
   * Test {@link ZipEncodingHelper#isUTF8(String)}.
   * <ul>
   *   <li>When {@code UTF8}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipEncodingHelper#isUTF8(String)}
   */
  @Test
  public void testIsUTF8_whenUtf8_thenReturnTrue2() {
    // Arrange, Act and Assert
    assertTrue(ZipEncodingHelper.isUTF8("UTF8"));
  }
}
