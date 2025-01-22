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

public class TailFilterDiffblueTest {
  /**
   * Test {@link TailFilter#TailFilter()}.
   * <p>
   * Method under test: {@link TailFilter#TailFilter()}
   */
  @Test
  public void testNewTailFilter() {
    // Arrange and Act
    TailFilter actualTailFilter = new TailFilter();

    // Assert
    assertNull(actualTailFilter.getParameters());
    assertNull(actualTailFilter.getProject());
    assertFalse(actualTailFilter.getInitialized());
  }

  /**
   * Test {@link TailFilter#read()}.
   * <ul>
   *   <li>Given {@link StringReader#StringReader(String)} with empty string.</li>
   *   <li>Then return minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link TailFilter#read()}
   */
  @Test
  public void testRead_givenStringReaderWithEmptyString_thenReturnMinusOne() throws IOException {
    // Arrange
    TailFilter tailFilter = new TailFilter(new StringReader(""));

    // Act and Assert
    assertEquals(-1, tailFilter.read());
    assertTrue(tailFilter.getInitialized());
  }

  /**
   * Test {@link TailFilter#read()}.
   * <ul>
   *   <li>Given {@link TailFilter#TailFilter(Reader)} with in is {@link StringReader#StringReader(String)} Skip is one.</li>
   *   <li>Then return minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link TailFilter#read()}
   */
  @Test
  public void testRead_givenTailFilterWithInIsStringReaderSkipIsOne_thenReturnMinusOne() throws IOException {
    // Arrange
    TailFilter tailFilter = new TailFilter(new StringReader("foo"));
    tailFilter.setSkip(1L);

    // Act and Assert
    assertEquals(-1, tailFilter.read());
    assertTrue(tailFilter.getInitialized());
  }

  /**
   * Test {@link TailFilter#read()}.
   * <ul>
   *   <li>Given {@link TailFilter#TailFilter(Reader)} with in is {@link StringReader#StringReader(String)} skip three.</li>
   *   <li>Then return minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link TailFilter#read()}
   */
  @Test
  public void testRead_givenTailFilterWithInIsStringReaderSkipThree_thenReturnMinusOne()
      throws IOException, IllegalArgumentException {
    // Arrange
    TailFilter tailFilter = new TailFilter(new StringReader("foo"));
    tailFilter.skip(3L);

    // Act and Assert
    assertEquals(-1, tailFilter.read());
    assertTrue(tailFilter.getInitialized());
  }

  /**
   * Test {@link TailFilter#read()}.
   * <ul>
   *   <li>Given {@link TailFilter#TailFilter(Reader)} with in is {@link StringReader#StringReader(String)}.</li>
   *   <li>Then return one hundred two.</li>
   * </ul>
   * <p>
   * Method under test: {@link TailFilter#read()}
   */
  @Test
  public void testRead_givenTailFilterWithInIsStringReader_thenReturnOneHundredTwo() throws IOException {
    // Arrange
    TailFilter tailFilter = new TailFilter(new StringReader("foo"));

    // Act and Assert
    assertEquals(102, tailFilter.read());
    assertTrue(tailFilter.getInitialized());
  }

  /**
   * Test {@link TailFilter#read()}.
   * <ul>
   *   <li>Then return one.</li>
   * </ul>
   * <p>
   * Method under test: {@link TailFilter#read()}
   */
  @Test
  public void testRead_thenReturnOne() throws IOException {
    // Arrange
    TailFilter tailFilter = new TailFilter(new CharArrayReader("\u0003\u0001\u0003\u0001".toCharArray(), 3, 3));

    // Act and Assert
    assertEquals(1, tailFilter.read());
    assertTrue(tailFilter.getInitialized());
  }

  /**
   * Test {@link TailFilter#read()}.
   * <ul>
   *   <li>Then return ten.</li>
   * </ul>
   * <p>
   * Method under test: {@link TailFilter#read()}
   */
  @Test
  public void testRead_thenReturnTen() throws IOException {
    // Arrange
    TailFilter tailFilter = new TailFilter(new CharArrayReader("\u0003\u0001\u0003\n".toCharArray(), 3, 3));

    // Act and Assert
    assertEquals(10, tailFilter.read());
    assertTrue(tailFilter.getInitialized());
  }

  /**
   * Test {@link TailFilter#read()}.
   * <ul>
   *   <li>Then return thirteen.</li>
   * </ul>
   * <p>
   * Method under test: {@link TailFilter#read()}
   */
  @Test
  public void testRead_thenReturnThirteen() throws IOException {
    // Arrange
    TailFilter tailFilter = new TailFilter(new CharArrayReader("\u0003\u0001\u0003\r".toCharArray(), 3, 3));

    // Act and Assert
    assertEquals(13, tailFilter.read());
    assertTrue(tailFilter.getInitialized());
  }

  /**
   * Test {@link TailFilter#chain(Reader)}.
   * <ul>
   *   <li>When {@link StringReader#StringReader(String)} with {@code foo}.</li>
   *   <li>Then return {@link TailFilter}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TailFilter#chain(Reader)}
   */
  @Test
  public void testChain_whenStringReaderWithFoo_thenReturnTailFilter() throws IOException {
    // Arrange
    TailFilter tailFilter = new TailFilter();

    // Act
    Reader actualChainResult = tailFilter.chain(new StringReader("foo"));

    // Assert
    assertTrue(actualChainResult instanceof TailFilter);
    assertEquals("foo", ((TailFilter) actualChainResult).readFully());
    assertNull(((TailFilter) actualChainResult).getParameters());
    assertNull(((TailFilter) actualChainResult).getProject());
    assertTrue(actualChainResult.ready());
    assertTrue(((TailFilter) actualChainResult).getInitialized());
  }
}
