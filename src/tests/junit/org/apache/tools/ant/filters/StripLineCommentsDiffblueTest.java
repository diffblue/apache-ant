package org.apache.tools.ant.filters;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.CharArrayReader;
import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import org.apache.tools.ant.filters.StripLineComments.Comment;
import org.junit.Test;

public class StripLineCommentsDiffblueTest {
  /**
   * Test Comment {@link Comment#addText(String)}.
   * <ul>
   *   <li>Given {@link Comment} (default constructor) Value is {@code foo}.</li>
   *   <li>Then throw {@link IllegalStateException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Comment#addText(String)}
   */
  @Test
  public void testCommentAddText_givenCommentValueIsFoo_thenThrowIllegalStateException() {
    // Arrange
    Comment comment = new Comment();
    comment.setValue("foo");

    // Act and Assert
    assertThrows(IllegalStateException.class, () -> comment.addText("Comment"));
  }

  /**
   * Test Comment {@link Comment#addText(String)}.
   * <ul>
   *   <li>Given {@link Comment} (default constructor).</li>
   *   <li>Then {@link Comment} (default constructor) Value is {@code Comment}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Comment#addText(String)}
   */
  @Test
  public void testCommentAddText_givenComment_thenCommentValueIsComment() {
    // Arrange
    Comment comment = new Comment();

    // Act
    comment.addText("Comment");

    // Assert
    assertEquals("Comment", comment.getValue());
  }

  /**
   * Test Comment getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link Comment}
   *   <li>{@link Comment#getValue()}
   * </ul>
   */
  @Test
  public void testCommentGettersAndSetters() {
    // Arrange, Act and Assert
    assertNull((new Comment()).getValue());
  }

  /**
   * Test Comment {@link Comment#setValue(String)}.
   * <ul>
   *   <li>Given {@link Comment} (default constructor) Value is {@code foo}.</li>
   *   <li>Then throw {@link IllegalStateException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Comment#setValue(String)}
   */
  @Test
  public void testCommentSetValue_givenCommentValueIsFoo_thenThrowIllegalStateException() {
    // Arrange
    Comment comment = new Comment();
    comment.setValue("foo");

    // Act and Assert
    assertThrows(IllegalStateException.class, () -> comment.setValue("Comment"));
  }

  /**
   * Test Comment {@link Comment#setValue(String)}.
   * <ul>
   *   <li>Given {@link Comment} (default constructor).</li>
   *   <li>Then {@link Comment} (default constructor) Value is {@code Comment}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Comment#setValue(String)}
   */
  @Test
  public void testCommentSetValue_givenComment_thenCommentValueIsComment() {
    // Arrange
    Comment comment = new Comment();

    // Act
    comment.setValue("Comment");

    // Assert
    assertEquals("Comment", comment.getValue());
  }

  /**
   * Test {@link StripLineComments#StripLineComments(Reader)}.
   * <p>
   * Method under test: {@link StripLineComments#StripLineComments(Reader)}
   */
  @Test
  public void testNewStripLineComments() {
    // Arrange and Act
    StripLineComments actualStripLineComments = new StripLineComments(new StringReader("foo"));

    // Assert
    assertNull(actualStripLineComments.getParameters());
    assertNull(actualStripLineComments.getProject());
    assertFalse(actualStripLineComments.getInitialized());
  }

  /**
   * Test {@link StripLineComments#StripLineComments()}.
   * <p>
   * Method under test: {@link StripLineComments#StripLineComments()}
   */
  @Test
  public void testNewStripLineComments2() {
    // Arrange and Act
    StripLineComments actualStripLineComments = new StripLineComments();

    // Assert
    assertNull(actualStripLineComments.getParameters());
    assertNull(actualStripLineComments.getProject());
    assertFalse(actualStripLineComments.getInitialized());
  }

  /**
   * Test {@link StripLineComments#read()}.
   * <ul>
   *   <li>Given {@link Comment} (default constructor) addText {@code Comment}.</li>
   *   <li>Then return one hundred two.</li>
   * </ul>
   * <p>
   * Method under test: {@link StripLineComments#read()}
   */
  @Test
  public void testRead_givenCommentAddTextComment_thenReturnOneHundredTwo() throws IOException {
    // Arrange
    Comment comment = new Comment();
    comment.addText("Comment");

    StripLineComments stripLineComments = new StripLineComments(new StringReader("foo"));
    stripLineComments.addConfiguredComment(comment);

    // Act and Assert
    assertEquals(102, stripLineComments.read());
    assertTrue(stripLineComments.getInitialized());
  }

  /**
   * Test {@link StripLineComments#read()}.
   * <ul>
   *   <li>Given {@link Comment} (default constructor) addText empty string.</li>
   *   <li>Then return minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link StripLineComments#read()}
   */
  @Test
  public void testRead_givenCommentAddTextEmptyString_thenReturnMinusOne() throws IOException {
    // Arrange
    Comment comment = new Comment();
    comment.addText("");

    StripLineComments stripLineComments = new StripLineComments(new StringReader("foo"));
    stripLineComments.addConfiguredComment(comment);

    // Act and Assert
    assertEquals(-1, stripLineComments.read());
    assertTrue(stripLineComments.getInitialized());
  }

  /**
   * Test {@link StripLineComments#read()}.
   * <ul>
   *   <li>Given {@link StringReader#StringReader(String)} with empty string.</li>
   *   <li>Then return minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link StripLineComments#read()}
   */
  @Test
  public void testRead_givenStringReaderWithEmptyString_thenReturnMinusOne() throws IOException {
    // Arrange
    StripLineComments stripLineComments = new StripLineComments(new StringReader(""));

    // Act and Assert
    assertEquals(-1, stripLineComments.read());
    assertTrue(stripLineComments.getInitialized());
  }

  /**
   * Test {@link StripLineComments#read()}.
   * <ul>
   *   <li>Given {@link StripLineComments#StripLineComments(Reader)} with in is {@link StringReader#StringReader(String)}.</li>
   *   <li>Then return one hundred two.</li>
   * </ul>
   * <p>
   * Method under test: {@link StripLineComments#read()}
   */
  @Test
  public void testRead_givenStripLineCommentsWithInIsStringReader_thenReturnOneHundredTwo() throws IOException {
    // Arrange
    StripLineComments stripLineComments = new StripLineComments(new StringReader("foo"));

    // Act and Assert
    assertEquals(102, stripLineComments.read());
    assertTrue(stripLineComments.getInitialized());
  }

  /**
   * Test {@link StripLineComments#read()}.
   * <ul>
   *   <li>Then return one.</li>
   * </ul>
   * <p>
   * Method under test: {@link StripLineComments#read()}
   */
  @Test
  public void testRead_thenReturnOne() throws IOException {
    // Arrange
    StripLineComments stripLineComments = new StripLineComments(
        new CharArrayReader("\u0003\u0001\u0003\u0001".toCharArray(), 3, 3));

    // Act and Assert
    assertEquals(1, stripLineComments.read());
    assertTrue(stripLineComments.getInitialized());
  }

  /**
   * Test {@link StripLineComments#read()}.
   * <ul>
   *   <li>Then return ten.</li>
   * </ul>
   * <p>
   * Method under test: {@link StripLineComments#read()}
   */
  @Test
  public void testRead_thenReturnTen() throws IOException {
    // Arrange
    StripLineComments stripLineComments = new StripLineComments(
        new CharArrayReader("\u0003\u0001\u0003\n".toCharArray(), 3, 3));

    // Act and Assert
    assertEquals(10, stripLineComments.read());
    assertTrue(stripLineComments.getInitialized());
  }

  /**
   * Test {@link StripLineComments#chain(Reader)}.
   * <ul>
   *   <li>When {@link StringReader#StringReader(String)} with {@code foo}.</li>
   *   <li>Then return {@link StripLineComments}.</li>
   * </ul>
   * <p>
   * Method under test: {@link StripLineComments#chain(Reader)}
   */
  @Test
  public void testChain_whenStringReaderWithFoo_thenReturnStripLineComments() throws IOException {
    // Arrange
    StripLineComments stripLineComments = new StripLineComments();

    // Act
    Reader actualChainResult = stripLineComments.chain(new StringReader("foo"));

    // Assert
    assertTrue(actualChainResult instanceof StripLineComments);
    assertEquals("foo", ((StripLineComments) actualChainResult).readFully());
    assertNull(((StripLineComments) actualChainResult).getParameters());
    assertNull(((StripLineComments) actualChainResult).getProject());
    assertTrue(actualChainResult.ready());
    assertTrue(((StripLineComments) actualChainResult).getInitialized());
  }
}
