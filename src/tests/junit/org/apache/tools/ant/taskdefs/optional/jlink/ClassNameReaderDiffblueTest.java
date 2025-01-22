package org.apache.tools.ant.taskdefs.optional.jlink;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import org.junit.Test;

public class ClassNameReaderDiffblueTest {
  /**
   * Test {@link ClassNameReader#getClassName(InputStream)}.
   * <ul>
   *   <li>When {@link ByteArrayInputStream#ByteArrayInputStream(byte[])} with {@code AXAXAXAX} Bytes is {@code UTF-8}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ClassNameReader#getClassName(InputStream)}
   */
  @Test
  public void testGetClassName_whenByteArrayInputStreamWithAxaxaxaxBytesIsUtf8_thenReturnNull() throws IOException {
    // Arrange
    ByteArrayInputStream input = new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8"));

    // Act and Assert
    assertNull(ClassNameReader.getClassName(input));
    byte[] byteArray = new byte[4];
    assertEquals(4, input.read(byteArray));
    assertArrayEquals("AXAX".getBytes("UTF-8"), byteArray);
  }
}
