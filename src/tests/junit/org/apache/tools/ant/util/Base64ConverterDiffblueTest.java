package org.apache.tools.ant.util;

import static org.junit.Assert.assertEquals;
import java.io.UnsupportedEncodingException;
import org.junit.Test;

public class Base64ConverterDiffblueTest {
  /**
   * Test {@link Base64Converter#encode(byte[])} with {@code octetString}.
   * <ul>
   *   <li>When {@code A}.</li>
   *   <li>Then return {@code QQFBAUEBQQFBAUEBQQFBAQ==}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Base64Converter#encode(byte[])}
   */
  @Test
  public void testEncodeWithOctetString_whenA_thenReturnQqfbauebqqfbauebqqfbaq() {
    // Arrange, Act and Assert
    assertEquals("QQFBAUEBQQFBAUEBQQFBAQ==",
        (new Base64Converter()).encode(new byte[]{'A', 1, 'A', 1, 'A', 1, 'A', 1, 'A', 1, 'A', 1, 'A', 1, 'A', 1}));
  }

  /**
   * Test {@link Base64Converter#encode(byte[])} with {@code octetString}.
   * <ul>
   *   <li>When {@code AXAXAXAX} Bytes is {@code UTF-8}.</li>
   *   <li>Then return {@code QVhBWEFYQVg=}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Base64Converter#encode(byte[])}
   */
  @Test
  public void testEncodeWithOctetString_whenAxaxaxaxBytesIsUtf8_thenReturnQVhBWEFYQVg()
      throws UnsupportedEncodingException {
    // Arrange
    Base64Converter base64Converter = new Base64Converter();

    // Act and Assert
    assertEquals("QVhBWEFYQVg=", base64Converter.encode("AXAXAXAX".getBytes("UTF-8")));
  }

  /**
   * Test {@link Base64Converter#encode(byte[])} with {@code octetString}.
   * <ul>
   *   <li>When empty array of {@code byte}.</li>
   *   <li>Then return null null null null.</li>
   * </ul>
   * <p>
   * Method under test: {@link Base64Converter#encode(byte[])}
   */
  @Test
  public void testEncodeWithOctetString_whenEmptyArrayOfByte_thenReturnNullNullNullNull() {
    // Arrange, Act and Assert
    assertEquals("\u0000\u0000\u0000\u0000", (new Base64Converter()).encode(new byte[]{}));
  }

  /**
   * Test {@link Base64Converter#encode(String)} with {@code s}.
   * <ul>
   *   <li>When {@code 42}.</li>
   *   <li>Then return {@code NDI=}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Base64Converter#encode(String)}
   */
  @Test
  public void testEncodeWithS_when42_thenReturnNdi() {
    // Arrange, Act and Assert
    assertEquals("NDI=", (new Base64Converter()).encode("42"));
  }

  /**
   * Test {@link Base64Converter#encode(String)} with {@code s}.
   * <ul>
   *   <li>When {@code 4242}.</li>
   *   <li>Then return {@code NDI0Mg==}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Base64Converter#encode(String)}
   */
  @Test
  public void testEncodeWithS_when4242_thenReturnNDI0Mg() {
    // Arrange, Act and Assert
    assertEquals("NDI0Mg==", (new Base64Converter()).encode("4242"));
  }

  /**
   * Test {@link Base64Converter#encode(String)} with {@code s}.
   * <ul>
   *   <li>When {@code foo}.</li>
   *   <li>Then return {@code Zm9v}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Base64Converter#encode(String)}
   */
  @Test
  public void testEncodeWithS_whenFoo_thenReturnZm9v() {
    // Arrange, Act and Assert
    assertEquals("Zm9v", (new Base64Converter()).encode("foo"));
  }
}
