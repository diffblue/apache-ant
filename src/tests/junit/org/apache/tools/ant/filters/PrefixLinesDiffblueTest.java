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

public class PrefixLinesDiffblueTest {
  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link PrefixLines#PrefixLines(Reader)}
   *   <li>{@link PrefixLines#setPrefix(String)}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    PrefixLines actualPrefixLines = new PrefixLines(new StringReader("foo"));
    actualPrefixLines.setPrefix("Prefix");

    // Assert
    assertNull(actualPrefixLines.getParameters());
    assertNull(actualPrefixLines.getProject());
    assertFalse(actualPrefixLines.getInitialized());
  }

  /**
   * Test {@link PrefixLines#PrefixLines()}.
   * <p>
   * Method under test: {@link PrefixLines#PrefixLines()}
   */
  @Test
  public void testNewPrefixLines() {
    // Arrange and Act
    PrefixLines actualPrefixLines = new PrefixLines();

    // Assert
    assertNull(actualPrefixLines.getParameters());
    assertNull(actualPrefixLines.getProject());
    assertFalse(actualPrefixLines.getInitialized());
  }

  /**
   * Test {@link PrefixLines#read()}.
   * <ul>
   *   <li>Given {@link PrefixLines#PrefixLines(Reader)} with in is {@link StringReader#StringReader(String)} Prefix is {@code Prefix}.</li>
   *   <li>Then return eighty.</li>
   * </ul>
   * <p>
   * Method under test: {@link PrefixLines#read()}
   */
  @Test
  public void testRead_givenPrefixLinesWithInIsStringReaderPrefixIsPrefix_thenReturnEighty() throws IOException {
    // Arrange
    PrefixLines prefixLines = new PrefixLines(new StringReader("foo"));
    prefixLines.setPrefix("Prefix");

    // Act and Assert
    assertEquals(80, prefixLines.read());
    assertTrue(prefixLines.getInitialized());
  }

  /**
   * Test {@link PrefixLines#read()}.
   * <ul>
   *   <li>Given {@link StringReader#StringReader(String)} with empty string.</li>
   *   <li>Then return minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link PrefixLines#read()}
   */
  @Test
  public void testRead_givenStringReaderWithEmptyString_thenReturnMinusOne() throws IOException {
    // Arrange
    PrefixLines prefixLines = new PrefixLines(new StringReader(""));

    // Act and Assert
    assertEquals(-1, prefixLines.read());
    assertTrue(prefixLines.getInitialized());
  }

  /**
   * Test {@link PrefixLines#read()}.
   * <ul>
   *   <li>Given {@link StringReader#StringReader(String)} with {@code foo}.</li>
   *   <li>Then return one hundred two.</li>
   * </ul>
   * <p>
   * Method under test: {@link PrefixLines#read()}
   */
  @Test
  public void testRead_givenStringReaderWithFoo_thenReturnOneHundredTwo() throws IOException {
    // Arrange
    PrefixLines prefixLines = new PrefixLines(new StringReader("foo"));

    // Act and Assert
    assertEquals(102, prefixLines.read());
    assertTrue(prefixLines.getInitialized());
  }

  /**
   * Test {@link PrefixLines#read()}.
   * <ul>
   *   <li>Then return one.</li>
   * </ul>
   * <p>
   * Method under test: {@link PrefixLines#read()}
   */
  @Test
  public void testRead_thenReturnOne() throws IOException {
    // Arrange
    PrefixLines prefixLines = new PrefixLines(new CharArrayReader("\u0003\u0001\u0003\u0001".toCharArray(), 3, 3));

    // Act and Assert
    assertEquals(1, prefixLines.read());
    assertTrue(prefixLines.getInitialized());
  }

  /**
   * Test {@link PrefixLines#read()}.
   * <ul>
   *   <li>Then return ten.</li>
   * </ul>
   * <p>
   * Method under test: {@link PrefixLines#read()}
   */
  @Test
  public void testRead_thenReturnTen() throws IOException {
    // Arrange
    PrefixLines prefixLines = new PrefixLines(new CharArrayReader("\u0003\u0001\u0003\n".toCharArray(), 3, 3));

    // Act and Assert
    assertEquals(10, prefixLines.read());
    assertTrue(prefixLines.getInitialized());
  }

  /**
   * Test {@link PrefixLines#chain(Reader)}.
   * <ul>
   *   <li>When {@link StringReader#StringReader(String)} with {@code foo}.</li>
   *   <li>Then return {@link PrefixLines}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PrefixLines#chain(Reader)}
   */
  @Test
  public void testChain_whenStringReaderWithFoo_thenReturnPrefixLines() throws IOException {
    // Arrange
    PrefixLines prefixLines = new PrefixLines();

    // Act
    Reader actualChainResult = prefixLines.chain(new StringReader("foo"));

    // Assert
    assertTrue(actualChainResult instanceof PrefixLines);
    assertEquals("foo", ((PrefixLines) actualChainResult).readFully());
    assertNull(((PrefixLines) actualChainResult).getParameters());
    assertNull(((PrefixLines) actualChainResult).getProject());
    assertTrue(actualChainResult.ready());
    assertTrue(((PrefixLines) actualChainResult).getInitialized());
  }
}
