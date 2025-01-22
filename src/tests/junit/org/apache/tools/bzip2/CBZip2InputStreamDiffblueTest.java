package org.apache.tools.bzip2;

import static org.junit.Assert.assertThrows;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import org.junit.Test;

public class CBZip2InputStreamDiffblueTest {
  /**
   * Test {@link CBZip2InputStream#CBZip2InputStream(InputStream)}.
   * <ul>
   *   <li>When {@code A}.</li>
   *   <li>Then throw {@link IOException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CBZip2InputStream#CBZip2InputStream(InputStream)}
   */
  @Test
  public void testNewCBZip2InputStream_whenA_thenThrowIOException() throws IOException {
    // Arrange, Act and Assert
    assertThrows(IOException.class,
        () -> new CBZip2InputStream(new ByteArrayInputStream(new byte[]{'h', 1, 'A', 'X', 'A', 'X', 'A', 'X'})));
  }

  /**
   * Test {@link CBZip2InputStream#CBZip2InputStream(InputStream, boolean)}.
   * <ul>
   *   <li>When {@code A}.</li>
   *   <li>Then throw {@link IOException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CBZip2InputStream#CBZip2InputStream(InputStream, boolean)}
   */
  @Test
  public void testNewCBZip2InputStream_whenA_thenThrowIOException2() throws IOException {
    // Arrange, Act and Assert
    assertThrows(IOException.class,
        () -> new CBZip2InputStream(new ByteArrayInputStream(new byte[]{'h', 1, 'A', 'X', 'A', 'X', 'A', 'X'}), true));

  }

  /**
   * Test {@link CBZip2InputStream#CBZip2InputStream(InputStream)}.
   * <ul>
   *   <li>When {@link ByteArrayInputStream#ByteArrayInputStream(byte[])} with {@code AXAXAXAX} Bytes is {@code UTF-8}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CBZip2InputStream#CBZip2InputStream(InputStream)}
   */
  @Test
  public void testNewCBZip2InputStream_whenByteArrayInputStreamWithAxaxaxaxBytesIsUtf8() throws IOException {
    // Arrange, Act and Assert
    assertThrows(IOException.class,
        () -> new CBZip2InputStream(new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8"))));
  }

  /**
   * Test {@link CBZip2InputStream#CBZip2InputStream(InputStream, boolean)}.
   * <ul>
   *   <li>When {@link ByteArrayInputStream#ByteArrayInputStream(byte[])} with {@code AXAXAXAX} Bytes is {@code UTF-8}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CBZip2InputStream#CBZip2InputStream(InputStream, boolean)}
   */
  @Test
  public void testNewCBZip2InputStream_whenByteArrayInputStreamWithAxaxaxaxBytesIsUtf82() throws IOException {
    // Arrange, Act and Assert
    assertThrows(IOException.class,
        () -> new CBZip2InputStream(new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8")), true));

  }

  /**
   * Test {@link CBZip2InputStream#CBZip2InputStream(InputStream)}.
   * <ul>
   *   <li>When {@link ByteArrayInputStream#ByteArrayInputStream(byte[])} with empty array of {@code byte}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CBZip2InputStream#CBZip2InputStream(InputStream)}
   */
  @Test
  public void testNewCBZip2InputStream_whenByteArrayInputStreamWithEmptyArrayOfByte() throws IOException {
    // Arrange, Act and Assert
    assertThrows(IOException.class, () -> new CBZip2InputStream(new ByteArrayInputStream(new byte[]{})));
  }

  /**
   * Test {@link CBZip2InputStream#CBZip2InputStream(InputStream, boolean)}.
   * <ul>
   *   <li>When {@link ByteArrayInputStream#ByteArrayInputStream(byte[])} with empty array of {@code byte}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CBZip2InputStream#CBZip2InputStream(InputStream, boolean)}
   */
  @Test
  public void testNewCBZip2InputStream_whenByteArrayInputStreamWithEmptyArrayOfByte2() throws IOException {
    // Arrange, Act and Assert
    assertThrows(IOException.class, () -> new CBZip2InputStream(new ByteArrayInputStream(new byte[]{}), true));

  }

  /**
   * Test {@link CBZip2InputStream#CBZip2InputStream(InputStream)}.
   * <ul>
   *   <li>When {@link ByteArrayInputStream#ByteArrayInputStream(byte[])} with {@code h1AXAXAX} Bytes is {@code UTF-8}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CBZip2InputStream#CBZip2InputStream(InputStream)}
   */
  @Test
  public void testNewCBZip2InputStream_whenByteArrayInputStreamWithH1AXAXAXBytesIsUtf8() throws IOException {
    // Arrange, Act and Assert
    assertThrows(IOException.class,
        () -> new CBZip2InputStream(new ByteArrayInputStream("h1AXAXAX".getBytes("UTF-8"))));
  }

  /**
   * Test {@link CBZip2InputStream#CBZip2InputStream(InputStream, boolean)}.
   * <ul>
   *   <li>When {@link ByteArrayInputStream#ByteArrayInputStream(byte[])} with {@code h1AXAXAX} Bytes is {@code UTF-8}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CBZip2InputStream#CBZip2InputStream(InputStream, boolean)}
   */
  @Test
  public void testNewCBZip2InputStream_whenByteArrayInputStreamWithH1AXAXAXBytesIsUtf82() throws IOException {
    // Arrange, Act and Assert
    assertThrows(IOException.class,
        () -> new CBZip2InputStream(new ByteArrayInputStream("h1AXAXAX".getBytes("UTF-8")), true));

  }

  /**
   * Test {@link CBZip2InputStream#CBZip2InputStream(InputStream)}.
   * <ul>
   *   <li>When {@link ByteArrayInputStream#ByteArrayInputStream(byte[])} with {@code h11AAXAX} Bytes is {@code UTF-8}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CBZip2InputStream#CBZip2InputStream(InputStream)}
   */
  @Test
  public void testNewCBZip2InputStream_whenByteArrayInputStreamWithH11AAXAXBytesIsUtf8() throws IOException {
    // Arrange, Act and Assert
    assertThrows(IOException.class,
        () -> new CBZip2InputStream(new ByteArrayInputStream("h11AAXAX".getBytes("UTF-8"))));
  }

  /**
   * Test {@link CBZip2InputStream#CBZip2InputStream(InputStream, boolean)}.
   * <ul>
   *   <li>When {@link ByteArrayInputStream#ByteArrayInputStream(byte[])} with {@code h11AAXAX} Bytes is {@code UTF-8}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CBZip2InputStream#CBZip2InputStream(InputStream, boolean)}
   */
  @Test
  public void testNewCBZip2InputStream_whenByteArrayInputStreamWithH11AAXAXBytesIsUtf82() throws IOException {
    // Arrange, Act and Assert
    assertThrows(IOException.class,
        () -> new CBZip2InputStream(new ByteArrayInputStream("h11AAXAX".getBytes("UTF-8")), true));

  }

  /**
   * Test {@link CBZip2InputStream#CBZip2InputStream(InputStream)}.
   * <ul>
   *   <li>When {@link ByteArrayInputStream#ByteArrayInputStream(byte[])} with {@code h11XAXAX} Bytes is {@code UTF-8}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CBZip2InputStream#CBZip2InputStream(InputStream)}
   */
  @Test
  public void testNewCBZip2InputStream_whenByteArrayInputStreamWithH11XAXAXBytesIsUtf8() throws IOException {
    // Arrange, Act and Assert
    assertThrows(IOException.class,
        () -> new CBZip2InputStream(new ByteArrayInputStream("h11XAXAX".getBytes("UTF-8"))));
  }

  /**
   * Test {@link CBZip2InputStream#CBZip2InputStream(InputStream, boolean)}.
   * <ul>
   *   <li>When {@link ByteArrayInputStream#ByteArrayInputStream(byte[])} with {@code h11XAXAX} Bytes is {@code UTF-8}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CBZip2InputStream#CBZip2InputStream(InputStream, boolean)}
   */
  @Test
  public void testNewCBZip2InputStream_whenByteArrayInputStreamWithH11XAXAXBytesIsUtf82() throws IOException {
    // Arrange, Act and Assert
    assertThrows(IOException.class,
        () -> new CBZip2InputStream(new ByteArrayInputStream("h11XAXAX".getBytes("UTF-8")), true));

  }

  /**
   * Test {@link CBZip2InputStream#CBZip2InputStream(InputStream)}.
   * <ul>
   *   <li>When {@link ByteArrayInputStream#ByteArrayInputStream(byte[])} with {@code hXAXAXAX} Bytes is {@code UTF-8}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CBZip2InputStream#CBZip2InputStream(InputStream)}
   */
  @Test
  public void testNewCBZip2InputStream_whenByteArrayInputStreamWithHXAXAXAXBytesIsUtf8() throws IOException {
    // Arrange, Act and Assert
    assertThrows(IOException.class,
        () -> new CBZip2InputStream(new ByteArrayInputStream("hXAXAXAX".getBytes("UTF-8"))));
  }

  /**
   * Test {@link CBZip2InputStream#CBZip2InputStream(InputStream, boolean)}.
   * <ul>
   *   <li>When {@link ByteArrayInputStream#ByteArrayInputStream(byte[])} with {@code hXAXAXAX} Bytes is {@code UTF-8}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CBZip2InputStream#CBZip2InputStream(InputStream, boolean)}
   */
  @Test
  public void testNewCBZip2InputStream_whenByteArrayInputStreamWithHXAXAXAXBytesIsUtf82() throws IOException {
    // Arrange, Act and Assert
    assertThrows(IOException.class,
        () -> new CBZip2InputStream(new ByteArrayInputStream("hXAXAXAX".getBytes("UTF-8")), true));

  }

  /**
   * Test {@link CBZip2InputStream#CBZip2InputStream(InputStream)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then throw {@link IOException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CBZip2InputStream#CBZip2InputStream(InputStream)}
   */
  @Test
  public void testNewCBZip2InputStream_whenNull_thenThrowIOException() throws IOException {
    // Arrange, Act and Assert
    assertThrows(IOException.class, () -> new CBZip2InputStream(null));
  }

  /**
   * Test {@link CBZip2InputStream#CBZip2InputStream(InputStream, boolean)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then throw {@link IOException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CBZip2InputStream#CBZip2InputStream(InputStream, boolean)}
   */
  @Test
  public void testNewCBZip2InputStream_whenNull_thenThrowIOException2() throws IOException {
    // Arrange, Act and Assert
    assertThrows(IOException.class, () -> new CBZip2InputStream(null, true));

  }
}
