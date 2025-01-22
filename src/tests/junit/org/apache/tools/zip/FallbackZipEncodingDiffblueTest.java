package org.apache.tools.zip;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import java.io.IOException;
import java.nio.ByteBuffer;
import org.junit.Test;

public class FallbackZipEncodingDiffblueTest {
  /**
   * Test {@link FallbackZipEncoding#canEncode(String)}.
   * <p>
   * Method under test: {@link FallbackZipEncoding#canEncode(String)}
   */
  @Test
  public void testCanEncode() {
    // Arrange, Act and Assert
    assertTrue((new FallbackZipEncoding("UTF-8")).canEncode("Name"));
  }

  /**
   * Test {@link FallbackZipEncoding#encode(String)}.
   * <ul>
   *   <li>Given {@link FallbackZipEncoding#FallbackZipEncoding(String)} with charset is {@code null}.</li>
   *   <li>Then return position is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link FallbackZipEncoding#encode(String)}
   */
  @Test
  public void testEncode_givenFallbackZipEncodingWithCharsetIsNull_thenReturnPositionIsZero() throws IOException {
    // Arrange and Act
    ByteBuffer actualEncodeResult = (new FallbackZipEncoding(null)).encode("Name");

    // Assert
    assertEquals(0, actualEncodeResult.position());
    assertEquals(4, actualEncodeResult.capacity());
    assertEquals(4, actualEncodeResult.limit());
    assertTrue(actualEncodeResult.hasRemaining());
    assertTrue(actualEncodeResult.hasArray());
    byte[] expectedArrayResult = "Name".getBytes("UTF-8");
    assertArrayEquals(expectedArrayResult, actualEncodeResult.array());
  }

  /**
   * Test {@link FallbackZipEncoding#encode(String)}.
   * <ul>
   *   <li>Given {@link FallbackZipEncoding#FallbackZipEncoding(String)} with charset is {@code UTF-8}.</li>
   *   <li>Then return position is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link FallbackZipEncoding#encode(String)}
   */
  @Test
  public void testEncode_givenFallbackZipEncodingWithCharsetIsUtf8_thenReturnPositionIsZero() throws IOException {
    // Arrange and Act
    ByteBuffer actualEncodeResult = (new FallbackZipEncoding("UTF-8")).encode("Name");

    // Assert
    assertEquals(0, actualEncodeResult.position());
    assertEquals(4, actualEncodeResult.capacity());
    assertEquals(4, actualEncodeResult.limit());
    assertTrue(actualEncodeResult.hasRemaining());
    assertTrue(actualEncodeResult.hasArray());
    byte[] expectedArrayResult = "Name".getBytes("UTF-8");
    assertArrayEquals(expectedArrayResult, actualEncodeResult.array());
  }

  /**
   * Test {@link FallbackZipEncoding#decode(byte[])}.
   * <ul>
   *   <li>Given {@link FallbackZipEncoding#FallbackZipEncoding(String)} with charset is {@code null}.</li>
   *   <li>Then return {@code AXAXAXAX}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FallbackZipEncoding#decode(byte[])}
   */
  @Test
  public void testDecode_givenFallbackZipEncodingWithCharsetIsNull_thenReturnAxaxaxax() throws IOException {
    // Arrange
    FallbackZipEncoding fallbackZipEncoding = new FallbackZipEncoding(null);

    // Act and Assert
    assertEquals("AXAXAXAX", fallbackZipEncoding.decode("AXAXAXAX".getBytes("UTF-8")));
  }

  /**
   * Test {@link FallbackZipEncoding#decode(byte[])}.
   * <ul>
   *   <li>Given {@link FallbackZipEncoding#FallbackZipEncoding(String)} with charset is {@code UTF-8}.</li>
   *   <li>Then return {@code AXAXAXAX}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FallbackZipEncoding#decode(byte[])}
   */
  @Test
  public void testDecode_givenFallbackZipEncodingWithCharsetIsUtf8_thenReturnAxaxaxax() throws IOException {
    // Arrange
    FallbackZipEncoding fallbackZipEncoding = new FallbackZipEncoding("UTF-8");

    // Act and Assert
    assertEquals("AXAXAXAX", fallbackZipEncoding.decode("AXAXAXAX".getBytes("UTF-8")));
  }
}
