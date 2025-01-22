package org.apache.tools.ant.filters;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import org.junit.Test;

public class StripJavaCommentsDiffblueTest {
  /**
   * Test {@link StripJavaComments#StripJavaComments(Reader)}.
   * <p>
   * Method under test: {@link StripJavaComments#StripJavaComments(Reader)}
   */
  @Test
  public void testNewStripJavaComments() {
    // Arrange and Act
    StripJavaComments actualStripJavaComments = new StripJavaComments(new StringReader("foo"));

    // Assert
    assertNull(actualStripJavaComments.getProject());
    assertFalse(actualStripJavaComments.getInitialized());
  }

  /**
   * Test {@link StripJavaComments#StripJavaComments()}.
   * <p>
   * Method under test: {@link StripJavaComments#StripJavaComments()}
   */
  @Test
  public void testNewStripJavaComments2() {
    // Arrange and Act
    StripJavaComments actualStripJavaComments = new StripJavaComments();

    // Assert
    assertNull(actualStripJavaComments.getProject());
    assertFalse(actualStripJavaComments.getInitialized());
  }

  /**
   * Test {@link StripJavaComments#read()}.
   * <ul>
   *   <li>Given {@link StringReader#StringReader(String)} with {@code foo}.</li>
   *   <li>Then return one hundred two.</li>
   * </ul>
   * <p>
   * Method under test: {@link StripJavaComments#read()}
   */
  @Test
  public void testRead_givenStringReaderWithFoo_thenReturnOneHundredTwo() throws IOException {
    // Arrange, Act and Assert
    assertEquals(102, (new StripJavaComments(new StringReader("foo"))).read());
  }

  /**
   * Test {@link StripJavaComments#chain(Reader)}.
   * <ul>
   *   <li>When {@link StringReader#StringReader(String)} with {@code foo}.</li>
   *   <li>Then return {@link StripJavaComments}.</li>
   * </ul>
   * <p>
   * Method under test: {@link StripJavaComments#chain(Reader)}
   */
  @Test
  public void testChain_whenStringReaderWithFoo_thenReturnStripJavaComments() throws IOException {
    // Arrange
    StripJavaComments stripJavaComments = new StripJavaComments();

    // Act
    Reader actualChainResult = stripJavaComments.chain(new StringReader("foo"));

    // Assert
    assertTrue(actualChainResult instanceof StripJavaComments);
    assertEquals("foo", ((StripJavaComments) actualChainResult).readFully());
    assertNull(((StripJavaComments) actualChainResult).getProject());
    assertFalse(((StripJavaComments) actualChainResult).getInitialized());
    assertTrue(actualChainResult.ready());
  }
}
