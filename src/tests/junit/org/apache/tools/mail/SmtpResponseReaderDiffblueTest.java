package org.apache.tools.mail;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import org.junit.Test;

public class SmtpResponseReaderDiffblueTest {
  /**
   * Test {@link SmtpResponseReader#getResponse()}.
   * <ul>
   *   <li>Given {@link ByteArrayInputStream#ByteArrayInputStream(byte[])} with {@code AXA-AXAX} Bytes is {@code UTF-8}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SmtpResponseReader#getResponse()}
   */
  @Test
  public void testGetResponse_givenByteArrayInputStreamWithAxaAxaxBytesIsUtf8() throws IOException {
    // Arrange, Act and Assert
    assertEquals("AXA AXAX",
        (new SmtpResponseReader(new ByteArrayInputStream("AXA-AXAX".getBytes("UTF-8")))).getResponse());
  }

  /**
   * Test {@link SmtpResponseReader#getResponse()}.
   * <ul>
   *   <li>Given {@link ByteArrayInputStream#ByteArrayInputStream(byte[])} with {@code AXAXAXAX} Bytes is {@code UTF-8}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SmtpResponseReader#getResponse()}
   */
  @Test
  public void testGetResponse_givenByteArrayInputStreamWithAxaxaxaxBytesIsUtf8() throws IOException {
    // Arrange, Act and Assert
    assertEquals("AXA AXAX",
        (new SmtpResponseReader(new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8")))).getResponse());
  }

  /**
   * Test {@link SmtpResponseReader#getResponse()}.
   * <ul>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link SmtpResponseReader#getResponse()}
   */
  @Test
  public void testGetResponse_thenReturnEmptyString() throws IOException {
    // Arrange, Act and Assert
    assertEquals("", (new SmtpResponseReader(new ByteArrayInputStream(new byte[]{}))).getResponse());
  }

  /**
   * Test {@link SmtpResponseReader#hasMoreLines(String)}.
   * <ul>
   *   <li>When {@code 42}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SmtpResponseReader#hasMoreLines(String)}
   */
  @Test
  public void testHasMoreLines_when42() throws UnsupportedEncodingException {
    // Arrange, Act and Assert
    assertFalse((new SmtpResponseReader(new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8")))).hasMoreLines("42"));
  }

  /**
   * Test {@link SmtpResponseReader#hasMoreLines(String)}.
   * <ul>
   *   <li>When {@code Line}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SmtpResponseReader#hasMoreLines(String)}
   */
  @Test
  public void testHasMoreLines_whenLine() throws UnsupportedEncodingException {
    // Arrange, Act and Assert
    assertFalse((new SmtpResponseReader(new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8")))).hasMoreLines("Line"));
  }
}
