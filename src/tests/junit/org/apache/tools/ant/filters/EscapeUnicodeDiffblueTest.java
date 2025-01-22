package org.apache.tools.ant.filters;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import org.junit.Test;

public class EscapeUnicodeDiffblueTest {
  /**
   * Test {@link EscapeUnicode#EscapeUnicode(Reader)}.
   * <p>
   * Method under test: {@link EscapeUnicode#EscapeUnicode(Reader)}
   */
  @Test
  public void testNewEscapeUnicode() {
    // Arrange and Act
    EscapeUnicode actualEscapeUnicode = new EscapeUnicode(new StringReader("foo"));

    // Assert
    assertNull(actualEscapeUnicode.getParameters());
    assertNull(actualEscapeUnicode.getProject());
    assertFalse(actualEscapeUnicode.getInitialized());
  }

  /**
   * Test {@link EscapeUnicode#EscapeUnicode()}.
   * <p>
   * Method under test: {@link EscapeUnicode#EscapeUnicode()}
   */
  @Test
  public void testNewEscapeUnicode2() {
    // Arrange and Act
    EscapeUnicode actualEscapeUnicode = new EscapeUnicode();

    // Assert
    assertNull(actualEscapeUnicode.getParameters());
    assertNull(actualEscapeUnicode.getProject());
    assertFalse(actualEscapeUnicode.getInitialized());
  }

  /**
   * Test {@link EscapeUnicode#read()}.
   * <ul>
   *   <li>Given {@link StringReader#StringReader(String)} with empty string.</li>
   *   <li>Then return minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link EscapeUnicode#read()}
   */
  @Test
  public void testRead_givenStringReaderWithEmptyString_thenReturnMinusOne() throws IOException {
    // Arrange
    EscapeUnicode escapeUnicode = new EscapeUnicode(new StringReader(""));

    // Act and Assert
    assertEquals(-1, escapeUnicode.read());
    assertTrue(escapeUnicode.getInitialized());
  }

  /**
   * Test {@link EscapeUnicode#read()}.
   * <ul>
   *   <li>Given {@link StringReader#StringReader(String)} with {@code foo}.</li>
   *   <li>Then return one hundred two.</li>
   * </ul>
   * <p>
   * Method under test: {@link EscapeUnicode#read()}
   */
  @Test
  public void testRead_givenStringReaderWithFoo_thenReturnOneHundredTwo() throws IOException {
    // Arrange
    EscapeUnicode escapeUnicode = new EscapeUnicode(new StringReader("foo"));

    // Act and Assert
    assertEquals(102, escapeUnicode.read());
    assertTrue(escapeUnicode.getInitialized());
  }

  /**
   * Test {@link EscapeUnicode#chain(Reader)}.
   * <ul>
   *   <li>When {@link StringReader#StringReader(String)} with {@code foo}.</li>
   *   <li>Then return {@link EscapeUnicode}.</li>
   * </ul>
   * <p>
   * Method under test: {@link EscapeUnicode#chain(Reader)}
   */
  @Test
  public void testChain_whenStringReaderWithFoo_thenReturnEscapeUnicode() throws IOException {
    // Arrange
    EscapeUnicode escapeUnicode = new EscapeUnicode();

    // Act
    Reader actualChainResult = escapeUnicode.chain(new StringReader("foo"));

    // Assert
    assertTrue(actualChainResult instanceof EscapeUnicode);
    assertEquals("foo", ((EscapeUnicode) actualChainResult).readFully());
    assertNull(((EscapeUnicode) actualChainResult).getParameters());
    assertNull(((EscapeUnicode) actualChainResult).getProject());
    assertTrue(actualChainResult.ready());
    assertTrue(((EscapeUnicode) actualChainResult).getInitialized());
  }
}
