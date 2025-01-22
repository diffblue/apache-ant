package org.apache.tools.zip;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.charset.Charset;
import org.junit.Test;

public class NioZipEncodingDiffblueTest {
  /**
   * Test {@link NioZipEncoding#canEncode(String)}.
   * <ul>
   *   <li>Given {@link NioZipEncoding#NioZipEncoding(Charset)} with charset is forName {@code UTF-8}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link NioZipEncoding#canEncode(String)}
   */
  @Test
  public void testCanEncode_givenNioZipEncodingWithCharsetIsForNameUtf8_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue((new NioZipEncoding(Charset.forName("UTF-8"))).canEncode("Name"));
  }

  /**
   * Test {@link NioZipEncoding#encode(String)}.
   * <ul>
   *   <li>Given {@link NioZipEncoding#NioZipEncoding(Charset)} with charset is forName {@code UTF-8}.</li>
   *   <li>Then return limit is four.</li>
   * </ul>
   * <p>
   * Method under test: {@link NioZipEncoding#encode(String)}
   */
  @Test
  public void testEncode_givenNioZipEncodingWithCharsetIsForNameUtf8_thenReturnLimitIsFour() {
    // Arrange and Act
    ByteBuffer actualEncodeResult = (new NioZipEncoding(Charset.forName("UTF-8"))).encode("Name");

    // Assert
    assertEquals(4, actualEncodeResult.limit());
    assertEquals(6, actualEncodeResult.capacity());
    assertTrue(actualEncodeResult.hasRemaining());
    assertArrayEquals(new byte[]{'N', 'a', 'm', 'e', 0, 0}, actualEncodeResult.array());
  }

  /**
   * Test {@link NioZipEncoding#encode(String)}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then return capacity is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link NioZipEncoding#encode(String)}
   */
  @Test
  public void testEncode_whenEmptyString_thenReturnCapacityIsZero() {
    // Arrange and Act
    ByteBuffer actualEncodeResult = (new NioZipEncoding(Charset.forName("UTF-8"))).encode("");

    // Assert
    assertEquals(0, actualEncodeResult.capacity());
    assertEquals(0, actualEncodeResult.limit());
    assertFalse(actualEncodeResult.hasRemaining());
    assertArrayEquals(new byte[]{}, actualEncodeResult.array());
  }

  /**
   * Test {@link NioZipEncoding#decode(byte[])}.
   * <ul>
   *   <li>Given {@link NioZipEncoding#NioZipEncoding(Charset)} with charset is forName {@code UTF-8}.</li>
   *   <li>Then return {@code AXAXAXAX}.</li>
   * </ul>
   * <p>
   * Method under test: {@link NioZipEncoding#decode(byte[])}
   */
  @Test
  public void testDecode_givenNioZipEncodingWithCharsetIsForNameUtf8_thenReturnAxaxaxax() throws IOException {
    // Arrange
    NioZipEncoding nioZipEncoding = new NioZipEncoding(Charset.forName("UTF-8"));

    // Act and Assert
    assertEquals("AXAXAXAX", nioZipEncoding.decode("AXAXAXAX".getBytes("UTF-8")));
  }
}
