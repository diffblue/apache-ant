package org.apache.tools.ant.util;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import org.junit.Test;

public class UUEncoderDiffblueTest {
  /**
   * Test {@link UUEncoder#encode(InputStream, OutputStream)}.
   * <p>
   * Method under test: {@link UUEncoder#encode(InputStream, OutputStream)}
   */
  @Test
  public void testEncode() throws IOException {
    // Arrange
    UUEncoder uuEncoder = new UUEncoder("Name");
    ByteArrayInputStream is = new ByteArrayInputStream(
        new byte[]{'A', -108, 'A', -108, 'A', -108, 'A', -108, 'A', -108, 'A', -108, 'A', -108, 'A', -108});
    ByteArrayOutputStream out = new ByteArrayOutputStream(1);

    // Act
    uuEncoder.encode(is, out);

    // Assert
    assertEquals(Retryable.RETRY_FOREVER, is.read(new byte[]{}));
    byte[] expectedToByteArrayResult = "begin 644 Name\n0091!E$&4091!E$&4091!E $!\n \nend\n".getBytes("UTF-8");
    assertArrayEquals(expectedToByteArrayResult, out.toByteArray());
  }

  /**
   * Test {@link UUEncoder#encode(InputStream, OutputStream)}.
   * <ul>
   *   <li>Then {@link ByteArrayInputStream#ByteArrayInputStream(byte[])} with {@code AXAXAXAX} Bytes is {@code UTF-8} read is {@link Retryable#RETRY_FOREVER}.</li>
   * </ul>
   * <p>
   * Method under test: {@link UUEncoder#encode(InputStream, OutputStream)}
   */
  @Test
  public void testEncode_thenByteArrayInputStreamWithAxaxaxaxBytesIsUtf8ReadIsRetry_forever() throws IOException {
    // Arrange
    UUEncoder uuEncoder = new UUEncoder("Name");
    ByteArrayInputStream is = new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8"));
    ByteArrayOutputStream out = new ByteArrayOutputStream(1);

    // Act
    uuEncoder.encode(is, out);

    // Assert
    assertEquals(Retryable.RETRY_FOREVER, is.read(new byte[]{}));
    byte[] expectedToByteArrayResult = "begin 644 Name\n(05A!6$%805@!\n \nend\n".getBytes("UTF-8");
    assertArrayEquals(expectedToByteArrayResult, out.toByteArray());
  }
}
