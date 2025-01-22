package org.apache.tools.ant.filters;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import java.io.CharArrayReader;
import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import org.apache.tools.ant.filters.LineContains.Contains;
import org.junit.Test;

public class LineContainsDiffblueTest {
  /**
   * Test Contains getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link Contains}
   *   <li>{@link Contains#setValue(String)}
   *   <li>{@link Contains#getValue()}
   * </ul>
   */
  @Test
  public void testContainsGettersAndSetters() {
    // Arrange and Act
    Contains actualContains = new Contains();
    actualContains.setValue("Contains");

    // Assert
    assertEquals("Contains", actualContains.getValue());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link LineContains#LineContains(Reader)}
   *   <li>{@link LineContains#setMatchAny(boolean)}
   *   <li>{@link LineContains#setNegate(boolean)}
   *   <li>{@link LineContains#isMatchAny()}
   *   <li>{@link LineContains#isNegated()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    LineContains actualLineContains = new LineContains(new StringReader("foo"));
    actualLineContains.setMatchAny(true);
    actualLineContains.setNegate(true);
    boolean actualIsMatchAnyResult = actualLineContains.isMatchAny();
    boolean actualIsNegatedResult = actualLineContains.isNegated();

    // Assert
    assertNull(actualLineContains.getParameters());
    assertNull(actualLineContains.getProject());
    assertFalse(actualLineContains.getInitialized());
    assertTrue(actualIsMatchAnyResult);
    assertTrue(actualIsNegatedResult);
  }

  /**
   * Test {@link LineContains#LineContains()}.
   * <p>
   * Method under test: {@link LineContains#LineContains()}
   */
  @Test
  public void testNewLineContains() {
    // Arrange and Act
    LineContains actualLineContains = new LineContains();

    // Assert
    assertNull(actualLineContains.getParameters());
    assertNull(actualLineContains.getProject());
    assertFalse(actualLineContains.getInitialized());
    assertFalse(actualLineContains.isMatchAny());
    assertFalse(actualLineContains.isNegated());
  }

  /**
   * Test {@link LineContains#read()}.
   * <ul>
   *   <li>Given {@link Contains} (default constructor) Value is {@code 42}.</li>
   *   <li>Then return minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link LineContains#read()}
   */
  @Test
  public void testRead_givenContainsValueIs42_thenReturnMinusOne() throws IOException {
    // Arrange
    Contains contains = new Contains();
    contains.setValue("42");

    LineContains lineContains = new LineContains(new StringReader("foo"));
    lineContains.addConfiguredContains(contains);

    // Act and Assert
    assertEquals(-1, lineContains.read());
    assertTrue(lineContains.getInitialized());
  }

  /**
   * Test {@link LineContains#read()}.
   * <ul>
   *   <li>Given {@link Contains} (default constructor) Value is empty string.</li>
   *   <li>Then return minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link LineContains#read()}
   */
  @Test
  public void testRead_givenContainsValueIsEmptyString_thenReturnMinusOne() throws IOException {
    // Arrange
    Contains contains = new Contains();
    contains.setValue("42");

    Contains contains2 = new Contains();
    contains2.setValue("");

    LineContains lineContains = new LineContains(new StringReader("foo"));
    lineContains.addConfiguredContains(contains2);
    lineContains.addConfiguredContains(contains);

    // Act and Assert
    assertEquals(-1, lineContains.read());
    assertTrue(lineContains.getInitialized());
  }

  /**
   * Test {@link LineContains#read()}.
   * <ul>
   *   <li>Given {@link Contains} (default constructor) Value is empty string.</li>
   *   <li>Then return one hundred two.</li>
   * </ul>
   * <p>
   * Method under test: {@link LineContains#read()}
   */
  @Test
  public void testRead_givenContainsValueIsEmptyString_thenReturnOneHundredTwo() throws IOException {
    // Arrange
    Contains contains = new Contains();
    contains.setValue("42");

    Contains contains2 = new Contains();
    contains2.setValue("");

    LineContains lineContains = new LineContains(new StringReader("foo"));
    lineContains.setMatchAny(true);
    lineContains.addConfiguredContains(contains2);
    lineContains.addConfiguredContains(contains);

    // Act and Assert
    assertEquals(102, lineContains.read());
    assertTrue(lineContains.getInitialized());
  }

  /**
   * Test {@link LineContains#read()}.
   * <ul>
   *   <li>Given {@link LineContains#LineContains(Reader)} with in is {@link StringReader#StringReader(String)} MatchAny is {@code true}.</li>
   *   <li>Then return minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link LineContains#read()}
   */
  @Test
  public void testRead_givenLineContainsWithInIsStringReaderMatchAnyIsTrue_thenReturnMinusOne() throws IOException {
    // Arrange
    Contains contains = new Contains();
    contains.setValue("42");

    LineContains lineContains = new LineContains(new StringReader("foo"));
    lineContains.setMatchAny(true);
    lineContains.addConfiguredContains(contains);

    // Act and Assert
    assertEquals(-1, lineContains.read());
    assertTrue(lineContains.getInitialized());
  }

  /**
   * Test {@link LineContains#read()}.
   * <ul>
   *   <li>Given {@link LineContains#LineContains(Reader)} with in is {@link StringReader#StringReader(String)}.</li>
   *   <li>Then return one hundred two.</li>
   * </ul>
   * <p>
   * Method under test: {@link LineContains#read()}
   */
  @Test
  public void testRead_givenLineContainsWithInIsStringReader_thenReturnOneHundredTwo() throws IOException {
    // Arrange
    LineContains lineContains = new LineContains(new StringReader("foo"));

    // Act and Assert
    assertEquals(102, lineContains.read());
    assertTrue(lineContains.getInitialized());
  }

  /**
   * Test {@link LineContains#read()}.
   * <ul>
   *   <li>Given {@link StringReader#StringReader(String)} with empty string.</li>
   *   <li>Then return minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link LineContains#read()}
   */
  @Test
  public void testRead_givenStringReaderWithEmptyString_thenReturnMinusOne() throws IOException {
    // Arrange
    LineContains lineContains = new LineContains(new StringReader(""));

    // Act and Assert
    assertEquals(-1, lineContains.read());
    assertTrue(lineContains.getInitialized());
  }

  /**
   * Test {@link LineContains#read()}.
   * <ul>
   *   <li>Then return one.</li>
   * </ul>
   * <p>
   * Method under test: {@link LineContains#read()}
   */
  @Test
  public void testRead_thenReturnOne() throws IOException {
    // Arrange
    LineContains lineContains = new LineContains(new CharArrayReader("\u0003\u0001\u0003\u0001".toCharArray(), 3, 3));

    // Act and Assert
    assertEquals(1, lineContains.read());
    assertTrue(lineContains.getInitialized());
  }

  /**
   * Test {@link LineContains#read()}.
   * <ul>
   *   <li>Then return ten.</li>
   * </ul>
   * <p>
   * Method under test: {@link LineContains#read()}
   */
  @Test
  public void testRead_thenReturnTen() throws IOException {
    // Arrange
    LineContains lineContains = new LineContains(new CharArrayReader("\u0003\u0001\u0003\n".toCharArray(), 3, 3));

    // Act and Assert
    assertEquals(10, lineContains.read());
    assertTrue(lineContains.getInitialized());
  }

  /**
   * Test {@link LineContains#chain(Reader)}.
   * <ul>
   *   <li>Given {@link LineContains#LineContains()} Negate is {@code true}.</li>
   *   <li>Then return Negated.</li>
   * </ul>
   * <p>
   * Method under test: {@link LineContains#chain(Reader)}
   */
  @Test
  public void testChain_givenLineContainsNegateIsTrue_thenReturnNegated() throws IOException {
    // Arrange
    LineContains lineContains = new LineContains();
    lineContains.setNegate(true);

    // Act
    Reader actualChainResult = lineContains.chain(new StringReader("foo"));

    // Assert
    assertTrue(actualChainResult instanceof LineContains);
    assertEquals("foo", ((LineContains) actualChainResult).readFully());
    assertNull(((LineContains) actualChainResult).getParameters());
    assertNull(((LineContains) actualChainResult).getProject());
    assertFalse(((LineContains) actualChainResult).getInitialized());
    assertFalse(((LineContains) actualChainResult).isMatchAny());
    assertTrue(actualChainResult.ready());
    assertTrue(((LineContains) actualChainResult).isNegated());
  }

  /**
   * Test {@link LineContains#chain(Reader)}.
   * <ul>
   *   <li>Given {@link LineContains#LineContains()}.</li>
   *   <li>When {@link StringReader#StringReader(String)} with {@code foo}.</li>
   *   <li>Then return not Negated.</li>
   * </ul>
   * <p>
   * Method under test: {@link LineContains#chain(Reader)}
   */
  @Test
  public void testChain_givenLineContains_whenStringReaderWithFoo_thenReturnNotNegated() throws IOException {
    // Arrange
    LineContains lineContains = new LineContains();

    // Act
    Reader actualChainResult = lineContains.chain(new StringReader("foo"));

    // Assert
    assertTrue(actualChainResult instanceof LineContains);
    assertEquals("foo", ((LineContains) actualChainResult).readFully());
    assertNull(((LineContains) actualChainResult).getParameters());
    assertNull(((LineContains) actualChainResult).getProject());
    assertFalse(((LineContains) actualChainResult).getInitialized());
    assertFalse(((LineContains) actualChainResult).isMatchAny());
    assertFalse(((LineContains) actualChainResult).isNegated());
    assertTrue(actualChainResult.ready());
  }
}
