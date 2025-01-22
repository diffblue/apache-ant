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

public class LineContainsRegExpDiffblueTest {
  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link LineContainsRegExp#LineContainsRegExp(Reader)}
   *   <li>{@link LineContainsRegExp#setNegate(boolean)}
   *   <li>{@link LineContainsRegExp#isNegated()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    LineContainsRegExp actualLineContainsRegExp = new LineContainsRegExp(new StringReader("foo"));
    actualLineContainsRegExp.setNegate(true);
    boolean actualIsNegatedResult = actualLineContainsRegExp.isNegated();

    // Assert
    assertNull(actualLineContainsRegExp.getParameters());
    assertNull(actualLineContainsRegExp.getProject());
    assertFalse(actualLineContainsRegExp.getInitialized());
    assertTrue(actualIsNegatedResult);
  }

  /**
   * Test {@link LineContainsRegExp#LineContainsRegExp()}.
   * <p>
   * Method under test: {@link LineContainsRegExp#LineContainsRegExp()}
   */
  @Test
  public void testNewLineContainsRegExp() {
    // Arrange and Act
    LineContainsRegExp actualLineContainsRegExp = new LineContainsRegExp();

    // Assert
    assertNull(actualLineContainsRegExp.getParameters());
    assertNull(actualLineContainsRegExp.getProject());
    assertFalse(actualLineContainsRegExp.getInitialized());
    assertFalse(actualLineContainsRegExp.isNegated());
  }

  /**
   * Test {@link LineContainsRegExp#read()}.
   * <ul>
   *   <li>Given {@link LineContainsRegExp#LineContainsRegExp(Reader)} with in is {@link StringReader#StringReader(String)}.</li>
   *   <li>Then return one hundred two.</li>
   * </ul>
   * <p>
   * Method under test: {@link LineContainsRegExp#read()}
   */
  @Test
  public void testRead_givenLineContainsRegExpWithInIsStringReader_thenReturnOneHundredTwo() throws IOException {
    // Arrange
    LineContainsRegExp lineContainsRegExp = new LineContainsRegExp(new StringReader("foo"));

    // Act and Assert
    assertEquals(102, lineContainsRegExp.read());
    assertTrue(lineContainsRegExp.getInitialized());
  }

  /**
   * Test {@link LineContainsRegExp#read()}.
   * <ul>
   *   <li>Given {@link StringReader#StringReader(String)} with empty string.</li>
   *   <li>Then return minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link LineContainsRegExp#read()}
   */
  @Test
  public void testRead_givenStringReaderWithEmptyString_thenReturnMinusOne() throws IOException {
    // Arrange
    LineContainsRegExp lineContainsRegExp = new LineContainsRegExp(new StringReader(""));

    // Act and Assert
    assertEquals(-1, lineContainsRegExp.read());
    assertTrue(lineContainsRegExp.getInitialized());
  }

  /**
   * Test {@link LineContainsRegExp#read()}.
   * <ul>
   *   <li>Then return one.</li>
   * </ul>
   * <p>
   * Method under test: {@link LineContainsRegExp#read()}
   */
  @Test
  public void testRead_thenReturnOne() throws IOException {
    // Arrange
    LineContainsRegExp lineContainsRegExp = new LineContainsRegExp(
        new CharArrayReader("\u0003\u0001\u0003\u0001".toCharArray(), 3, 3));

    // Act and Assert
    assertEquals(1, lineContainsRegExp.read());
    assertTrue(lineContainsRegExp.getInitialized());
  }

  /**
   * Test {@link LineContainsRegExp#chain(Reader)}.
   * <ul>
   *   <li>Given {@link LineContainsRegExp#LineContainsRegExp()} Negate is {@code true}.</li>
   *   <li>Then return Negated.</li>
   * </ul>
   * <p>
   * Method under test: {@link LineContainsRegExp#chain(Reader)}
   */
  @Test
  public void testChain_givenLineContainsRegExpNegateIsTrue_thenReturnNegated() throws IOException {
    // Arrange
    LineContainsRegExp lineContainsRegExp = new LineContainsRegExp();
    lineContainsRegExp.setNegate(true);

    // Act
    Reader actualChainResult = lineContainsRegExp.chain(new StringReader("foo"));

    // Assert
    assertTrue(actualChainResult instanceof LineContainsRegExp);
    assertEquals("foo", ((LineContainsRegExp) actualChainResult).readFully());
    assertNull(((LineContainsRegExp) actualChainResult).getParameters());
    assertNull(((LineContainsRegExp) actualChainResult).getProject());
    assertFalse(((LineContainsRegExp) actualChainResult).getInitialized());
    assertTrue(actualChainResult.ready());
    assertTrue(((LineContainsRegExp) actualChainResult).isNegated());
  }

  /**
   * Test {@link LineContainsRegExp#chain(Reader)}.
   * <ul>
   *   <li>Given {@link LineContainsRegExp#LineContainsRegExp()}.</li>
   *   <li>When {@link StringReader#StringReader(String)} with {@code foo}.</li>
   *   <li>Then return not Negated.</li>
   * </ul>
   * <p>
   * Method under test: {@link LineContainsRegExp#chain(Reader)}
   */
  @Test
  public void testChain_givenLineContainsRegExp_whenStringReaderWithFoo_thenReturnNotNegated() throws IOException {
    // Arrange
    LineContainsRegExp lineContainsRegExp = new LineContainsRegExp();

    // Act
    Reader actualChainResult = lineContainsRegExp.chain(new StringReader("foo"));

    // Assert
    assertTrue(actualChainResult instanceof LineContainsRegExp);
    assertEquals("foo", ((LineContainsRegExp) actualChainResult).readFully());
    assertNull(((LineContainsRegExp) actualChainResult).getParameters());
    assertNull(((LineContainsRegExp) actualChainResult).getProject());
    assertFalse(((LineContainsRegExp) actualChainResult).getInitialized());
    assertFalse(((LineContainsRegExp) actualChainResult).isNegated());
    assertTrue(actualChainResult.ready());
  }
}
