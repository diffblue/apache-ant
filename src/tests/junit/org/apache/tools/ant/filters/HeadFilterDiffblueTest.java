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

public class HeadFilterDiffblueTest {
  /**
   * Test {@link HeadFilter#HeadFilter()}.
   * <p>
   * Method under test: {@link HeadFilter#HeadFilter()}
   */
  @Test
  public void testNewHeadFilter() {
    // Arrange and Act
    HeadFilter actualHeadFilter = new HeadFilter();

    // Assert
    assertNull(actualHeadFilter.getParameters());
    assertNull(actualHeadFilter.getProject());
    assertFalse(actualHeadFilter.getInitialized());
  }

  /**
   * Test {@link HeadFilter#read()}.
   * <ul>
   *   <li>Given {@link HeadFilter#HeadFilter(Reader)} with in is {@link StringReader#StringReader(String)} Skip is {@link Long#MIN_VALUE}.</li>
   *   <li>Then return minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link HeadFilter#read()}
   */
  @Test
  public void testRead_givenHeadFilterWithInIsStringReaderSkipIsMin_value_thenReturnMinusOne() throws IOException {
    // Arrange
    HeadFilter headFilter = new HeadFilter(new StringReader("foo"));
    headFilter.setSkip(Long.MIN_VALUE);

    // Act and Assert
    assertEquals(-1, headFilter.read());
    assertTrue(headFilter.getInitialized());
  }

  /**
   * Test {@link HeadFilter#read()}.
   * <ul>
   *   <li>Given {@link HeadFilter#HeadFilter(Reader)} with in is {@link StringReader#StringReader(String)} Skip is three.</li>
   *   <li>Then return minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link HeadFilter#read()}
   */
  @Test
  public void testRead_givenHeadFilterWithInIsStringReaderSkipIsThree_thenReturnMinusOne() throws IOException {
    // Arrange
    HeadFilter headFilter = new HeadFilter(new StringReader("foo"));
    headFilter.setSkip(3L);

    // Act and Assert
    assertEquals(-1, headFilter.read());
    assertTrue(headFilter.getInitialized());
  }

  /**
   * Test {@link HeadFilter#read()}.
   * <ul>
   *   <li>Given {@link HeadFilter#HeadFilter(Reader)} with in is {@link StringReader#StringReader(String)}.</li>
   *   <li>Then return one hundred two.</li>
   * </ul>
   * <p>
   * Method under test: {@link HeadFilter#read()}
   */
  @Test
  public void testRead_givenHeadFilterWithInIsStringReader_thenReturnOneHundredTwo() throws IOException {
    // Arrange
    HeadFilter headFilter = new HeadFilter(new StringReader("foo"));

    // Act and Assert
    assertEquals(102, headFilter.read());
    assertTrue(headFilter.getInitialized());
  }

  /**
   * Test {@link HeadFilter#read()}.
   * <ul>
   *   <li>Given {@link StringReader#StringReader(String)} with empty string.</li>
   *   <li>Then return minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link HeadFilter#read()}
   */
  @Test
  public void testRead_givenStringReaderWithEmptyString_thenReturnMinusOne() throws IOException {
    // Arrange
    HeadFilter headFilter = new HeadFilter(new StringReader(""));

    // Act and Assert
    assertEquals(-1, headFilter.read());
    assertTrue(headFilter.getInitialized());
  }

  /**
   * Test {@link HeadFilter#read()}.
   * <ul>
   *   <li>Then return one.</li>
   * </ul>
   * <p>
   * Method under test: {@link HeadFilter#read()}
   */
  @Test
  public void testRead_thenReturnOne() throws IOException {
    // Arrange
    HeadFilter headFilter = new HeadFilter(new CharArrayReader("\u0003\u0001\u0003\u0001".toCharArray(), 3, 3));

    // Act and Assert
    assertEquals(1, headFilter.read());
    assertTrue(headFilter.getInitialized());
  }

  /**
   * Test {@link HeadFilter#read()}.
   * <ul>
   *   <li>Then return ten.</li>
   * </ul>
   * <p>
   * Method under test: {@link HeadFilter#read()}
   */
  @Test
  public void testRead_thenReturnTen() throws IOException {
    // Arrange
    HeadFilter headFilter = new HeadFilter(new CharArrayReader("\u0003\u0001\u0003\n".toCharArray(), 3, 3));

    // Act and Assert
    assertEquals(10, headFilter.read());
    assertTrue(headFilter.getInitialized());
  }

  /**
   * Test {@link HeadFilter#read()}.
   * <ul>
   *   <li>Then return thirteen.</li>
   * </ul>
   * <p>
   * Method under test: {@link HeadFilter#read()}
   */
  @Test
  public void testRead_thenReturnThirteen() throws IOException {
    // Arrange
    HeadFilter headFilter = new HeadFilter(new CharArrayReader("\u0003\u0001\u0003\r".toCharArray(), 3, 3));

    // Act and Assert
    assertEquals(13, headFilter.read());
    assertTrue(headFilter.getInitialized());
  }

  /**
   * Test {@link HeadFilter#chain(Reader)}.
   * <ul>
   *   <li>When {@link StringReader#StringReader(String)} with {@code foo}.</li>
   *   <li>Then return {@link HeadFilter}.</li>
   * </ul>
   * <p>
   * Method under test: {@link HeadFilter#chain(Reader)}
   */
  @Test
  public void testChain_whenStringReaderWithFoo_thenReturnHeadFilter() throws IOException {
    // Arrange
    HeadFilter headFilter = new HeadFilter();

    // Act
    Reader actualChainResult = headFilter.chain(new StringReader("foo"));

    // Assert
    assertTrue(actualChainResult instanceof HeadFilter);
    assertEquals("foo", ((HeadFilter) actualChainResult).readFully());
    assertNull(((HeadFilter) actualChainResult).getParameters());
    assertNull(((HeadFilter) actualChainResult).getProject());
    assertTrue(actualChainResult.ready());
    assertTrue(((HeadFilter) actualChainResult).getInitialized());
  }
}
