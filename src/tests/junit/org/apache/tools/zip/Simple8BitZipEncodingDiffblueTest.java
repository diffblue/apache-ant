package org.apache.tools.zip;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.nio.ByteBuffer;
import org.junit.Test;

public class Simple8BitZipEncodingDiffblueTest {
  /**
   * Test {@link Simple8BitZipEncoding#Simple8BitZipEncoding(char[])}.
   * <p>
   * Method under test: {@link Simple8BitZipEncoding#Simple8BitZipEncoding(char[])}
   */
  @Test
  public void testNewSimple8BitZipEncoding() {
    // Arrange and Act
    Simple8BitZipEncoding actualSimple8BitZipEncoding = new Simple8BitZipEncoding("AZAZ".toCharArray());

    // Assert
    assertEquals('A', actualSimple8BitZipEncoding.decodeByte((byte) 'A'));
    assertTrue(actualSimple8BitZipEncoding.canEncode("Name"));
    assertTrue(actualSimple8BitZipEncoding.canEncodeChar('A'));
  }

  /**
   * Test {@link Simple8BitZipEncoding#decodeByte(byte)}.
   * <ul>
   *   <li>When {@code A}.</li>
   *   <li>Then return {@code A}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Simple8BitZipEncoding#decodeByte(byte)}
   */
  @Test
  public void testDecodeByte_whenA_thenReturnA() {
    // Arrange, Act and Assert
    assertEquals('A', (new Simple8BitZipEncoding("AZAZ".toCharArray())).decodeByte((byte) 'A'));
  }

  /**
   * Test {@link Simple8BitZipEncoding#decodeByte(byte)}.
   * <ul>
   *   <li>When {@link Byte#MIN_VALUE}.</li>
   *   <li>Then return {@code A}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Simple8BitZipEncoding#decodeByte(byte)}
   */
  @Test
  public void testDecodeByte_whenMin_value_thenReturnA() {
    // Arrange, Act and Assert
    assertEquals('A', (new Simple8BitZipEncoding("AZAZ".toCharArray())).decodeByte(Byte.MIN_VALUE));
  }

  /**
   * Test {@link Simple8BitZipEncoding#canEncodeChar(char)}.
   * <p>
   * Method under test: {@link Simple8BitZipEncoding#canEncodeChar(char)}
   */
  @Test
  public void testCanEncodeChar() {
    // Arrange, Act and Assert
    assertTrue((new Simple8BitZipEncoding("ZAZ".toCharArray())).canEncodeChar(''));
  }

  /**
   * Test {@link Simple8BitZipEncoding#canEncodeChar(char)}.
   * <ul>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Simple8BitZipEncoding#canEncodeChar(char)}
   */
  @Test
  public void testCanEncodeChar_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new Simple8BitZipEncoding("AZAZ".toCharArray())).canEncodeChar(''));
  }

  /**
   * Test {@link Simple8BitZipEncoding#canEncodeChar(char)}.
   * <ul>
   *   <li>When {@code A}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Simple8BitZipEncoding#canEncodeChar(char)}
   */
  @Test
  public void testCanEncodeChar_whenA_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue((new Simple8BitZipEncoding("AZAZ".toCharArray())).canEncodeChar('A'));
  }

  /**
   * Test {@link Simple8BitZipEncoding#pushEncodedChar(ByteBuffer, char)}.
   * <ul>
   *   <li>Then wrap {@code AXAXAXAX} Bytes is {@code UTF-8} position is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Simple8BitZipEncoding#pushEncodedChar(ByteBuffer, char)}
   */
  @Test
  public void testPushEncodedChar_thenWrapAxaxaxaxBytesIsUtf8PositionIsOne() throws UnsupportedEncodingException {
    // Arrange
    Simple8BitZipEncoding simple8BitZipEncoding = new Simple8BitZipEncoding("AZAZ".toCharArray());
    ByteBuffer bb = ByteBuffer.wrap("AXAXAXAX".getBytes("UTF-8"));

    // Act
    boolean actualPushEncodedCharResult = simple8BitZipEncoding.pushEncodedChar(bb, 'A');

    // Assert
    assertEquals(1, bb.position());
    assertTrue(actualPushEncodedCharResult);
  }

  /**
   * Test {@link Simple8BitZipEncoding#canEncode(String)}.
   * <p>
   * Method under test: {@link Simple8BitZipEncoding#canEncode(String)}
   */
  @Test
  public void testCanEncode() {
    // Arrange, Act and Assert
    assertTrue((new Simple8BitZipEncoding("AZAZ".toCharArray())).canEncode("Name"));
  }

  /**
   * Test {@link Simple8BitZipEncoding#encode(String)}.
   * <p>
   * Method under test: {@link Simple8BitZipEncoding#encode(String)}
   */
  @Test
  public void testEncode() {
    // Arrange and Act
    ByteBuffer actualEncodeResult = (new Simple8BitZipEncoding("AZAZ".toCharArray())).encode("Name");

    // Assert
    assertEquals(0, actualEncodeResult.position());
    assertEquals(12, actualEncodeResult.capacity());
    assertEquals(4, actualEncodeResult.limit());
    assertTrue(actualEncodeResult.hasRemaining());
    assertTrue(actualEncodeResult.hasArray());
    assertArrayEquals(new byte[]{'N', 'a', 'm', 'e', 0, 0, 0, 0, 0, 0, 0, 0}, actualEncodeResult.array());
  }

  /**
   * Test {@link Simple8BitZipEncoding#decode(byte[])}.
   * <ul>
   *   <li>When array of {@code byte} with {@link Byte#MIN_VALUE} and {@code X}.</li>
   *   <li>Then return {@code AXAXAXAX}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Simple8BitZipEncoding#decode(byte[])}
   */
  @Test
  public void testDecode_whenArrayOfByteWithMin_valueAndX_thenReturnAxaxaxax() throws IOException {
    // Arrange, Act and Assert
    assertEquals("AXAXAXAX", (new Simple8BitZipEncoding("AZAZ".toCharArray()))
        .decode(new byte[]{Byte.MIN_VALUE, 'X', 'A', 'X', 'A', 'X', 'A', 'X'}));
  }

  /**
   * Test {@link Simple8BitZipEncoding#decode(byte[])}.
   * <ul>
   *   <li>When {@code AXAXAXAX} Bytes is {@code UTF-8}.</li>
   *   <li>Then return {@code AXAXAXAX}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Simple8BitZipEncoding#decode(byte[])}
   */
  @Test
  public void testDecode_whenAxaxaxaxBytesIsUtf8_thenReturnAxaxaxax() throws IOException {
    // Arrange
    Simple8BitZipEncoding simple8BitZipEncoding = new Simple8BitZipEncoding("AZAZ".toCharArray());

    // Act and Assert
    assertEquals("AXAXAXAX", simple8BitZipEncoding.decode("AXAXAXAX".getBytes("UTF-8")));
  }
}
