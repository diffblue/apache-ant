package org.apache.tools.ant.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringReader;
import java.nio.charset.CharsetEncoder;
import org.junit.Test;

public class ReaderInputStreamDiffblueTest {
  /**
  * Method under test: {@link ReaderInputStream#ReaderInputStream(Reader)}
  */
  @Test
  public void testConstructor() throws IOException {
    // Arrange
    StringReader stringReader = new StringReader("foo");

    // Act and Assert
    assertEquals(0, (new ReaderInputStream(stringReader)).available());
    assertTrue(stringReader.ready());
  }

  /**
   * Method under test: {@link ReaderInputStream#ReaderInputStream(Reader, String)}
   */
  @Test
  public void testConstructor2() throws IOException {
    // Arrange
    StringReader stringReader = new StringReader("foo");

    // Act and Assert
    assertEquals(0, (new ReaderInputStream(stringReader, "UTF-8")).available());
    assertTrue(stringReader.ready());
  }

  /**
   * Method under test: {@link ReaderInputStream#ReaderInputStream(Reader, CharsetEncoder)}
   */
  @Test
  public void testConstructor3() throws IOException {
    // Arrange
    StringReader stringReader = new StringReader("foo");

    // Act and Assert
    assertEquals(0, (new ReaderInputStream(stringReader, (CharsetEncoder) null)).available());
    assertTrue(stringReader.ready());
  }

  /**
   * Method under test: {@link ReaderInputStream#ReaderInputStream(Reader, CharsetEncoder, int)}
   */
  @Test
  public void testConstructor4() throws IOException {
    // Arrange
    StringReader stringReader = new StringReader("foo");

    // Act and Assert
    assertEquals(0, (new ReaderInputStream(stringReader, null, 3)).available());
    assertTrue(stringReader.ready());
  }

  /**
   * Method under test: {@link ReaderInputStream#read()}
   */
  @Test
  public void testRead() throws IOException {
    // Arrange, Act and Assert
    assertEquals(102, (new ReaderInputStream(new StringReader("foo"))).read());
    assertEquals(Retryable.RETRY_FOREVER, (new ReaderInputStream(new StringReader(""))).read());
    assertEquals(0, (new ReaderInputStream(new StringReader("foo"))).read(new byte[]{}));
    assertEquals(0, (new ReaderInputStream(new StringReader("foo")))
        .read(new byte[]{'A', 'A', 'A', 'A', 'A', 'A', 'A', 'A'}, 0, 0));
  }

  /**
   * Method under test: {@link ReaderInputStream#read(byte[])}
   */
  @Test
  public void testRead2() throws IOException {
    // Arrange
    ReaderInputStream readerInputStream = new ReaderInputStream(new StringReader("foo"));

    // Act and Assert
    assertEquals(3, readerInputStream.read("AAAAAAAA".getBytes("UTF-8")));
  }

  /**
   * Method under test: {@link ReaderInputStream#read(byte[])}
   */
  @Test
  public void testRead3() throws IOException {
    // Arrange
    ReaderInputStream readerInputStream = new ReaderInputStream(new StringReader(""));

    // Act and Assert
    assertEquals(Retryable.RETRY_FOREVER, readerInputStream.read("AAAAAAAA".getBytes("UTF-8")));
  }

  /**
   * Method under test: {@link ReaderInputStream#read(byte[])}
   */
  @Test
  public void testRead4() throws IOException {
    // Arrange
    ReaderInputStream readerInputStream = new ReaderInputStream(
        new InputStreamReader(new ByteArrayInputStream("AAAAAAAA".getBytes("UTF-8"))));

    // Act and Assert
    assertEquals(8, readerInputStream.read("AAAAAAAA".getBytes("UTF-8")));
  }

  /**
   * Method under test: {@link ReaderInputStream#read(byte[], int, int)}
   */
  @Test
  public void testRead5() throws IOException {
    // Arrange
    ReaderInputStream readerInputStream = new ReaderInputStream(new StringReader("foo"));

    // Act and Assert
    assertEquals(3, readerInputStream.read("AAAAAAAA".getBytes("UTF-8"), 1, 3));
  }

  /**
   * Method under test: {@link ReaderInputStream#read(byte[], int, int)}
   */
  @Test
  public void testRead6() throws IOException {
    // Arrange
    ReaderInputStream readerInputStream = new ReaderInputStream(new StringReader("42"));

    // Act and Assert
    assertEquals(2, readerInputStream.read("AAAAAAAA".getBytes("UTF-8"), 1, 3));
  }

  /**
   * Method under test: {@link ReaderInputStream#read(byte[], int, int)}
   */
  @Test
  public void testRead7() throws IOException {
    // Arrange
    ReaderInputStream readerInputStream = new ReaderInputStream(new StringReader(""));

    // Act and Assert
    assertEquals(Retryable.RETRY_FOREVER, readerInputStream.read("AAAAAAAA".getBytes("UTF-8"), 1, 3));
  }
}

