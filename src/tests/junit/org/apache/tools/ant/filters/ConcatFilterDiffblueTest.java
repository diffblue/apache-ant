package org.apache.tools.ant.filters;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.nio.file.Paths;
import org.junit.Test;

public class ConcatFilterDiffblueTest {
  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link ConcatFilter#ConcatFilter(Reader)}
   *   <li>{@link ConcatFilter#setAppend(File)}
   *   <li>{@link ConcatFilter#setPrepend(File)}
   *   <li>{@link ConcatFilter#getAppend()}
   *   <li>{@link ConcatFilter#getPrepend()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    ConcatFilter actualConcatFilter = new ConcatFilter(new StringReader("foo"));
    File append = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    actualConcatFilter.setAppend(append);
    File prepend = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    actualConcatFilter.setPrepend(prepend);
    File actualAppend = actualConcatFilter.getAppend();
    File actualPrepend = actualConcatFilter.getPrepend();

    // Assert
    assertNull(actualConcatFilter.getParameters());
    assertNull(actualConcatFilter.getProject());
    assertFalse(actualConcatFilter.getInitialized());
    assertSame(append, actualAppend);
    assertSame(prepend, actualPrepend);
  }

  /**
   * Test {@link ConcatFilter#ConcatFilter()}.
   * <p>
   * Method under test: {@link ConcatFilter#ConcatFilter()}
   */
  @Test
  public void testNewConcatFilter() {
    // Arrange and Act
    ConcatFilter actualConcatFilter = new ConcatFilter();

    // Assert
    assertNull(actualConcatFilter.getParameters());
    assertNull(actualConcatFilter.getAppend());
    assertNull(actualConcatFilter.getPrepend());
    assertNull(actualConcatFilter.getProject());
    assertFalse(actualConcatFilter.getInitialized());
  }

  /**
   * Test {@link ConcatFilter#read()}.
   * <ul>
   *   <li>Given {@link StringReader#StringReader(String)} with empty string.</li>
   *   <li>Then return minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link ConcatFilter#read()}
   */
  @Test
  public void testRead_givenStringReaderWithEmptyString_thenReturnMinusOne() throws IOException {
    // Arrange
    ConcatFilter concatFilter = new ConcatFilter(new StringReader(""));

    // Act and Assert
    assertEquals(-1, concatFilter.read());
    assertTrue(concatFilter.getInitialized());
  }

  /**
   * Test {@link ConcatFilter#read()}.
   * <ul>
   *   <li>Given {@link StringReader#StringReader(String)} with {@code foo}.</li>
   *   <li>Then return one hundred two.</li>
   * </ul>
   * <p>
   * Method under test: {@link ConcatFilter#read()}
   */
  @Test
  public void testRead_givenStringReaderWithFoo_thenReturnOneHundredTwo() throws IOException {
    // Arrange
    ConcatFilter concatFilter = new ConcatFilter(new StringReader("foo"));

    // Act and Assert
    assertEquals(102, concatFilter.read());
    assertTrue(concatFilter.getInitialized());
  }

  /**
   * Test {@link ConcatFilter#chain(Reader)}.
   * <ul>
   *   <li>When {@link StringReader#StringReader(String)} with {@code foo}.</li>
   *   <li>Then return {@link ConcatFilter}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ConcatFilter#chain(Reader)}
   */
  @Test
  public void testChain_whenStringReaderWithFoo_thenReturnConcatFilter() throws IOException {
    // Arrange
    ConcatFilter concatFilter = new ConcatFilter();

    // Act
    Reader actualChainResult = concatFilter.chain(new StringReader("foo"));

    // Assert
    assertTrue(actualChainResult instanceof ConcatFilter);
    assertEquals("foo", ((ConcatFilter) actualChainResult).readFully());
    assertNull(((ConcatFilter) actualChainResult).getParameters());
    assertNull(((ConcatFilter) actualChainResult).getAppend());
    assertNull(((ConcatFilter) actualChainResult).getPrepend());
    assertNull(((ConcatFilter) actualChainResult).getProject());
    assertFalse(((ConcatFilter) actualChainResult).getInitialized());
    assertTrue(actualChainResult.ready());
  }
}
