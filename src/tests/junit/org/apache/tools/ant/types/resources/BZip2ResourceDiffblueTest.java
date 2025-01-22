package org.apache.tools.ant.types.resources;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import org.apache.tools.bzip2.CBZip2OutputStream;
import org.junit.Test;

public class BZip2ResourceDiffblueTest {
  /**
   * Test {@link BZip2Resource#wrapStream(InputStream)} with {@code in}.
   * <ul>
   *   <li>When {@link ByteArrayInputStream#ByteArrayInputStream(byte[])} with {@code AXAXAXAX} Bytes is {@code UTF-8}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BZip2Resource#wrapStream(InputStream)}
   */
  @Test
  public void testWrapStreamWithIn_whenByteArrayInputStreamWithAxaxaxaxBytesIsUtf8() throws IOException {
    // Arrange
    BZip2Resource bZip2Resource = new BZip2Resource();

    // Act and Assert
    assertThrows(IOException.class,
        () -> bZip2Resource.wrapStream(new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8"))));
  }

  /**
   * Test {@link BZip2Resource#wrapStream(InputStream)} with {@code in}.
   * <ul>
   *   <li>When {@link ByteArrayInputStream#ByteArrayInputStream(byte[])} with {@code BXAXAXAX} Bytes is {@code UTF-8}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BZip2Resource#wrapStream(InputStream)}
   */
  @Test
  public void testWrapStreamWithIn_whenByteArrayInputStreamWithBxaxaxaxBytesIsUtf8() throws IOException {
    // Arrange
    BZip2Resource bZip2Resource = new BZip2Resource();

    // Act and Assert
    assertThrows(IOException.class,
        () -> bZip2Resource.wrapStream(new ByteArrayInputStream("BXAXAXAX".getBytes("UTF-8"))));
  }

  /**
   * Test {@link BZip2Resource#wrapStream(OutputStream)} with {@code out}.
   * <ul>
   *   <li>Then return {@link CBZip2OutputStream}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BZip2Resource#wrapStream(OutputStream)}
   */
  @Test
  public void testWrapStreamWithOut_thenReturnCBZip2OutputStream() throws IOException {
    // Arrange
    BZip2Resource bZip2Resource = new BZip2Resource();
    ByteArrayOutputStream out = new ByteArrayOutputStream(1);

    // Act
    OutputStream actualWrapStreamResult = bZip2Resource.wrapStream(out);

    // Assert
    assertTrue(actualWrapStreamResult instanceof CBZip2OutputStream);
    assertEquals(9, ((CBZip2OutputStream) actualWrapStreamResult).getBlockSize());
    byte[] expectedToByteArrayResult = "BZh".getBytes("UTF-8");
    assertArrayEquals(expectedToByteArrayResult, out.toByteArray());
  }

  /**
   * Test {@link BZip2Resource#getCompressionName()}.
   * <p>
   * Method under test: {@link BZip2Resource#getCompressionName()}
   */
  @Test
  public void testGetCompressionName() {
    // Arrange, Act and Assert
    assertEquals("Bzip2", (new BZip2Resource()).getCompressionName());
  }
}
