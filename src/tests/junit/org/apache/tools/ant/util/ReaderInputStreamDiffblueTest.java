package org.apache.tools.ant.util;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import java.io.CharArrayReader;
import java.io.IOException;
import java.io.StringReader;
import org.junit.Test;

public class ReaderInputStreamDiffblueTest {
  /**
   * Test {@link ReaderInputStream#read(byte[], int, int)} with {@code array}, {@code off}, {@code len}.
   * <ul>
   *   <li>Given {@link StringReader#StringReader(String)} with {@code 42}.</li>
   *   <li>Then return two.</li>
   * </ul>
   * <p>
   * Method under test: {@link ReaderInputStream#read(byte[], int, int)}
   */
  @Test
  public void testReadWithArrayOffLen_givenStringReaderWith42_thenReturnTwo() throws IOException {
    // Arrange
    ReaderInputStream readerInputStream = new ReaderInputStream(new StringReader("42"));
    byte[] array = "AXAXAXAX".getBytes("UTF-8");

    // Act and Assert
    assertEquals(2, readerInputStream.read(array, 1, 3));
    assertArrayEquals("A42XAXAX".getBytes("UTF-8"), array);
  }

  /**
   * Test {@link ReaderInputStream#read(byte[], int, int)} with {@code array}, {@code off}, {@code len}.
   * <ul>
   *   <li>Given {@link StringReader#StringReader(String)} with empty string.</li>
   *   <li>Then return {@link Retryable#RETRY_FOREVER}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ReaderInputStream#read(byte[], int, int)}
   */
  @Test
  public void testReadWithArrayOffLen_givenStringReaderWithEmptyString_thenReturnRetry_forever() throws IOException {
    // Arrange
    ReaderInputStream readerInputStream = new ReaderInputStream(new StringReader(""));
    byte[] array = "AXAXAXAX".getBytes("UTF-8");

    // Act and Assert
    assertEquals(Retryable.RETRY_FOREVER, readerInputStream.read(array, 1, 3));
    assertArrayEquals("AXAXAXAX".getBytes("UTF-8"), array);
  }

  /**
   * Test {@link ReaderInputStream#read(byte[], int, int)} with {@code array}, {@code off}, {@code len}.
   * <ul>
   *   <li>Given {@link StringReader#StringReader(String)} with {@code foo}.</li>
   *   <li>Then return three.</li>
   * </ul>
   * <p>
   * Method under test: {@link ReaderInputStream#read(byte[], int, int)}
   */
  @Test
  public void testReadWithArrayOffLen_givenStringReaderWithFoo_thenReturnThree() throws IOException {
    // Arrange
    ReaderInputStream readerInputStream = new ReaderInputStream(new StringReader("foo"));
    byte[] array = "AXAXAXAX".getBytes("UTF-8");

    // Act and Assert
    assertEquals(3, readerInputStream.read(array, 1, 3));
    assertArrayEquals("AfooAXAX".getBytes("UTF-8"), array);
  }

  /**
   * Test {@link ReaderInputStream#read(byte[], int, int)} with {@code array}, {@code off}, {@code len}.
   * <ul>
   *   <li>Given {@link StringReader#StringReader(String)} with {@code foo}.</li>
   *   <li>When zero.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link ReaderInputStream#read(byte[], int, int)}
   */
  @Test
  public void testReadWithArrayOffLen_givenStringReaderWithFoo_whenZero_thenReturnZero() throws IOException {
    // Arrange
    ReaderInputStream readerInputStream = new ReaderInputStream(new StringReader("foo"));
    byte[] array = "AXAXAXAX".getBytes("UTF-8");

    // Act and Assert
    assertEquals(0, readerInputStream.read(array, 1, 0));
    assertArrayEquals("AXAXAXAX".getBytes("UTF-8"), array);
  }

  /**
   * Test {@link ReaderInputStream#read(byte[])} with {@code b}.
   * <ul>
   *   <li>Given {@link CharArrayReader#CharArrayReader(char[])} with ￿ backspace ￿ toCharArray.</li>
   *   <li>Then return eight.</li>
   * </ul>
   * <p>
   * Method under test: {@link ReaderInputStream#read(byte[])}
   */
  @Test
  public void testReadWithB_givenCharArrayReaderWithBackspaceToCharArray_thenReturnEight() throws IOException {
    // Arrange
    ReaderInputStream readerInputStream = new ReaderInputStream(new CharArrayReader("\b￿\b￿".toCharArray()));
    byte[] b = "AXAXAXAX".getBytes("UTF-8");

    // Act and Assert
    assertEquals(8, readerInputStream.read(b));
    assertArrayEquals(new byte[]{'\b', -17, -65, -65, '\b', -17, -65, -65}, b);
  }

  /**
   * Test {@link ReaderInputStream#read(byte[])} with {@code b}.
   * <ul>
   *   <li>Given {@link StringReader#StringReader(String)} with empty string.</li>
   *   <li>Then return {@link Retryable#RETRY_FOREVER}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ReaderInputStream#read(byte[])}
   */
  @Test
  public void testReadWithB_givenStringReaderWithEmptyString_thenReturnRetry_forever() throws IOException {
    // Arrange
    ReaderInputStream readerInputStream = new ReaderInputStream(new StringReader(""));
    byte[] b = "AXAXAXAX".getBytes("UTF-8");

    // Act and Assert
    assertEquals(Retryable.RETRY_FOREVER, readerInputStream.read(b));
    assertArrayEquals("AXAXAXAX".getBytes("UTF-8"), b);
  }

  /**
   * Test {@link ReaderInputStream#read(byte[])} with {@code b}.
   * <ul>
   *   <li>Given {@link StringReader#StringReader(String)} with {@code foo}.</li>
   *   <li>When {@code AXAXAXAX} Bytes is {@code UTF-8}.</li>
   *   <li>Then return three.</li>
   * </ul>
   * <p>
   * Method under test: {@link ReaderInputStream#read(byte[])}
   */
  @Test
  public void testReadWithB_givenStringReaderWithFoo_whenAxaxaxaxBytesIsUtf8_thenReturnThree() throws IOException {
    // Arrange
    ReaderInputStream readerInputStream = new ReaderInputStream(new StringReader("foo"));
    byte[] b = "AXAXAXAX".getBytes("UTF-8");

    // Act and Assert
    assertEquals(3, readerInputStream.read(b));
    assertArrayEquals("fooXAXAX".getBytes("UTF-8"), b);
  }

  /**
   * Test {@link ReaderInputStream#read(byte[])} with {@code b}.
   * <ul>
   *   <li>Given {@link StringReader#StringReader(String)} with {@code foo}.</li>
   *   <li>When empty array of {@code byte}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link ReaderInputStream#read(byte[])}
   */
  @Test
  public void testReadWithB_givenStringReaderWithFoo_whenEmptyArrayOfByte_thenReturnZero() throws IOException {
    // Arrange
    byte[] b = new byte[]{};

    // Act and Assert
    assertEquals(0, (new ReaderInputStream(new StringReader("foo"))).read(b));
    assertArrayEquals(new byte[]{}, b);
  }

  /**
   * Test {@link ReaderInputStream#read()}.
   * <ul>
   *   <li>Given {@link StringReader#StringReader(String)} with empty string.</li>
   *   <li>Then return {@link Retryable#RETRY_FOREVER}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ReaderInputStream#read()}
   */
  @Test
  public void testRead_givenStringReaderWithEmptyString_thenReturnRetry_forever() throws IOException {
    // Arrange, Act and Assert
    assertEquals(Retryable.RETRY_FOREVER, (new ReaderInputStream(new StringReader(""))).read());
  }

  /**
   * Test {@link ReaderInputStream#read()}.
   * <ul>
   *   <li>Given {@link StringReader#StringReader(String)} with {@code foo}.</li>
   *   <li>Then return one hundred two.</li>
   * </ul>
   * <p>
   * Method under test: {@link ReaderInputStream#read()}
   */
  @Test
  public void testRead_givenStringReaderWithFoo_thenReturnOneHundredTwo() throws IOException {
    // Arrange, Act and Assert
    assertEquals(102, (new ReaderInputStream(new StringReader("foo"))).read());
  }
}
