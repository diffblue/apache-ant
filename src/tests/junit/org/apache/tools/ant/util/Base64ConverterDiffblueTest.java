package org.apache.tools.ant.util;

import static org.junit.Assert.assertEquals;
import java.io.UnsupportedEncodingException;
import org.junit.Test;

public class Base64ConverterDiffblueTest {
  /**
  * Method under test: {@link Base64Converter#encode(String)}
  */
  @Test
  public void testEncode() {
    // Arrange, Act and Assert
    assertEquals("Zm9v", (new Base64Converter()).encode("foo"));
    assertEquals("NDI=", (new Base64Converter()).encode("42"));
    assertEquals("NDI0Mg==", (new Base64Converter()).encode("4242"));
    assertEquals("\u0000\u0000\u0000\u0000", (new Base64Converter()).encode(new byte[]{}));
  }

  /**
   * Method under test: {@link Base64Converter#encode(byte[])}
   */
  @Test
  public void testEncode2() throws UnsupportedEncodingException {
    // Arrange
    Base64Converter base64Converter = new Base64Converter();

    // Act and Assert
    assertEquals("QUFBQUFBQUE=", base64Converter.encode("AAAAAAAA".getBytes("UTF-8")));
  }

  /**
   * Method under test: {@link Base64Converter#encode(byte[])}
   */
  @Test
  public void testEncode3() throws UnsupportedEncodingException {
    // Arrange
    Base64Converter base64Converter = new Base64Converter();

    // Act and Assert
    assertEquals("QUFBQUFBQUFBQUFBQUFBQQ==", base64Converter.encode("AAAAAAAAAAAAAAAA".getBytes("UTF-8")));
  }
}

