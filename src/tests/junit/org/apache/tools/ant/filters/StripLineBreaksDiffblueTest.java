package org.apache.tools.ant.filters;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import org.junit.Test;

public class StripLineBreaksDiffblueTest {
  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link StripLineBreaks#StripLineBreaks(Reader)}
   *   <li>{@link StripLineBreaks#setLineBreaks(String)}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    StripLineBreaks actualStripLineBreaks = new StripLineBreaks(new StringReader("foo"));
    actualStripLineBreaks.setLineBreaks("Line Breaks");

    // Assert
    assertNull(actualStripLineBreaks.getParameters());
    assertNull(actualStripLineBreaks.getProject());
    assertFalse(actualStripLineBreaks.getInitialized());
  }

  /**
   * Test {@link StripLineBreaks#StripLineBreaks()}.
   * <p>
   * Method under test: {@link StripLineBreaks#StripLineBreaks()}
   */
  @Test
  public void testNewStripLineBreaks() {
    // Arrange and Act
    StripLineBreaks actualStripLineBreaks = new StripLineBreaks();

    // Assert
    assertNull(actualStripLineBreaks.getParameters());
    assertNull(actualStripLineBreaks.getProject());
    assertFalse(actualStripLineBreaks.getInitialized());
  }

  /**
   * Test {@link StripLineBreaks#read()}.
   * <ul>
   *   <li>Given {@link StringReader#StringReader(String)} with empty string.</li>
   *   <li>Then return minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link StripLineBreaks#read()}
   */
  @Test
  public void testRead_givenStringReaderWithEmptyString_thenReturnMinusOne() throws IOException {
    // Arrange
    StripLineBreaks stripLineBreaks = new StripLineBreaks(new StringReader(""));

    // Act and Assert
    assertEquals(-1, stripLineBreaks.read());
    assertTrue(stripLineBreaks.getInitialized());
  }

  /**
   * Test {@link StripLineBreaks#read()}.
   * <ul>
   *   <li>Given {@link StringReader#StringReader(String)} with {@code foo}.</li>
   *   <li>Then return one hundred two.</li>
   * </ul>
   * <p>
   * Method under test: {@link StripLineBreaks#read()}
   */
  @Test
  public void testRead_givenStringReaderWithFoo_thenReturnOneHundredTwo() throws IOException {
    // Arrange
    StripLineBreaks stripLineBreaks = new StripLineBreaks(new StringReader("foo"));

    // Act and Assert
    assertEquals(102, stripLineBreaks.read());
    assertTrue(stripLineBreaks.getInitialized());
  }

  /**
   * Test {@link StripLineBreaks#chain(Reader)}.
   * <ul>
   *   <li>When {@link StringReader#StringReader(String)} with {@code foo}.</li>
   *   <li>Then return {@link StripLineBreaks}.</li>
   * </ul>
   * <p>
   * Method under test: {@link StripLineBreaks#chain(Reader)}
   */
  @Test
  public void testChain_whenStringReaderWithFoo_thenReturnStripLineBreaks() throws IOException {
    // Arrange
    StripLineBreaks stripLineBreaks = new StripLineBreaks();

    // Act
    Reader actualChainResult = stripLineBreaks.chain(new StringReader("foo"));

    // Assert
    assertTrue(actualChainResult instanceof StripLineBreaks);
    assertEquals("foo", ((StripLineBreaks) actualChainResult).readFully());
    assertNull(((StripLineBreaks) actualChainResult).getParameters());
    assertNull(((StripLineBreaks) actualChainResult).getProject());
    assertTrue(actualChainResult.ready());
    assertTrue(((StripLineBreaks) actualChainResult).getInitialized());
  }
}
