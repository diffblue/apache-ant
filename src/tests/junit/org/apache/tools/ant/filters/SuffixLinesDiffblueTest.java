package org.apache.tools.ant.filters;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import java.io.CharArrayReader;
import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import org.junit.Test;

public class SuffixLinesDiffblueTest {
  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link SuffixLines#SuffixLines(Reader)}
   *   <li>{@link SuffixLines#setSuffix(String)}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    SuffixLines actualSuffixLines = new SuffixLines(new StringReader("foo"));
    actualSuffixLines.setSuffix("Suffix");

    // Assert
    assertNull(actualSuffixLines.getParameters());
    assertNull(actualSuffixLines.getProject());
    assertFalse(actualSuffixLines.getInitialized());
  }

  /**
   * Test {@link SuffixLines#SuffixLines()}.
   * <p>
   * Method under test: {@link SuffixLines#SuffixLines()}
   */
  @Test
  public void testNewSuffixLines() {
    // Arrange and Act
    SuffixLines actualSuffixLines = new SuffixLines();

    // Assert
    assertNull(actualSuffixLines.getParameters());
    assertNull(actualSuffixLines.getProject());
    assertFalse(actualSuffixLines.getInitialized());
  }

  /**
   * Test {@link SuffixLines#read()}.
   * <ul>
   *   <li>Given {@link StringReader#StringReader(String)} with cr lf.</li>
   *   <li>Then return eighty-three.</li>
   * </ul>
   * <p>
   * Method under test: {@link SuffixLines#read()}
   */
  @Test
  public void testRead_givenStringReaderWithCrLf_thenReturnEightyThree() throws IOException {
    // Arrange
    SuffixLines suffixLines = new SuffixLines(new StringReader("\r\n"));
    suffixLines.setSuffix("Suffix");

    // Act and Assert
    assertEquals(83, suffixLines.read());
    assertTrue(suffixLines.getInitialized());
  }

  /**
   * Test {@link SuffixLines#read()}.
   * <ul>
   *   <li>Given {@link StringReader#StringReader(String)} with empty string.</li>
   *   <li>Then return minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link SuffixLines#read()}
   */
  @Test
  public void testRead_givenStringReaderWithEmptyString_thenReturnMinusOne() throws IOException {
    // Arrange
    SuffixLines suffixLines = new SuffixLines(new StringReader(""));

    // Act and Assert
    assertEquals(-1, suffixLines.read());
    assertTrue(suffixLines.getInitialized());
  }

  /**
   * Test {@link SuffixLines#read()}.
   * <ul>
   *   <li>Given {@link StringReader#StringReader(String)} with {@code foo}.</li>
   *   <li>Then return one hundred two.</li>
   * </ul>
   * <p>
   * Method under test: {@link SuffixLines#read()}
   */
  @Test
  public void testRead_givenStringReaderWithFoo_thenReturnOneHundredTwo() throws IOException {
    // Arrange
    SuffixLines suffixLines = new SuffixLines(new StringReader("foo"));

    // Act and Assert
    assertEquals(102, suffixLines.read());
    assertTrue(suffixLines.getInitialized());
  }

  /**
   * Test {@link SuffixLines#read()}.
   * <ul>
   *   <li>Given {@link StringReader#StringReader(String)} with {@code foo}.</li>
   *   <li>Then return one hundred two.</li>
   * </ul>
   * <p>
   * Method under test: {@link SuffixLines#read()}
   */
  @Test
  public void testRead_givenStringReaderWithFoo_thenReturnOneHundredTwo2() throws IOException {
    // Arrange
    SuffixLines suffixLines = new SuffixLines(new StringReader("foo"));
    suffixLines.setSuffix("Suffix");

    // Act and Assert
    assertEquals(102, suffixLines.read());
    assertTrue(suffixLines.getInitialized());
  }

  /**
   * Test {@link SuffixLines#read()}.
   * <ul>
   *   <li>Given {@link StringReader#StringReader(String)} with lf.</li>
   *   <li>Then return eighty-three.</li>
   * </ul>
   * <p>
   * Method under test: {@link SuffixLines#read()}
   */
  @Test
  public void testRead_givenStringReaderWithLf_thenReturnEightyThree() throws IOException {
    // Arrange
    SuffixLines suffixLines = new SuffixLines(new StringReader("\n"));
    suffixLines.setSuffix("Suffix");

    // Act and Assert
    assertEquals(83, suffixLines.read());
    assertTrue(suffixLines.getInitialized());
  }

  /**
   * Test {@link SuffixLines#read()}.
   * <ul>
   *   <li>Then return one.</li>
   * </ul>
   * <p>
   * Method under test: {@link SuffixLines#read()}
   */
  @Test
  public void testRead_thenReturnOne() throws IOException {
    // Arrange
    SuffixLines suffixLines = new SuffixLines(new CharArrayReader("\u0003\u0001\u0003\u0001".toCharArray(), 3, 3));

    // Act and Assert
    assertEquals(1, suffixLines.read());
    assertTrue(suffixLines.getInitialized());
  }

  /**
   * Test {@link SuffixLines#read()}.
   * <ul>
   *   <li>Then return ten.</li>
   * </ul>
   * <p>
   * Method under test: {@link SuffixLines#read()}
   */
  @Test
  public void testRead_thenReturnTen() throws IOException {
    // Arrange
    SuffixLines suffixLines = new SuffixLines(new CharArrayReader("\u0003\u0001\u0003\n".toCharArray(), 3, 3));

    // Act and Assert
    assertEquals(10, suffixLines.read());
    assertTrue(suffixLines.getInitialized());
  }

  /**
   * Test {@link SuffixLines#chain(Reader)}.
   * <ul>
   *   <li>When {@link StringReader#StringReader(String)} with {@code foo}.</li>
   *   <li>Then return {@link SuffixLines}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SuffixLines#chain(Reader)}
   */
  @Test
  public void testChain_whenStringReaderWithFoo_thenReturnSuffixLines() throws IOException {
    // Arrange
    SuffixLines suffixLines = new SuffixLines();

    // Act
    Reader actualChainResult = suffixLines.chain(new StringReader("foo"));

    // Assert
    assertTrue(actualChainResult instanceof SuffixLines);
    assertEquals("foo", ((SuffixLines) actualChainResult).readFully());
    assertNull(((SuffixLines) actualChainResult).getParameters());
    assertNull(((SuffixLines) actualChainResult).getProject());
    assertTrue(actualChainResult.ready());
    assertTrue(((SuffixLines) actualChainResult).getInitialized());
  }
}
