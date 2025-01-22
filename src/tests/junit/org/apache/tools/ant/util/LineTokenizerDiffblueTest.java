package org.apache.tools.ant.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import org.apache.tools.ant.Location;
import org.junit.Test;

public class LineTokenizerDiffblueTest {
  /**
   * Test {@link LineTokenizer#getToken(Reader)}.
   * <ul>
   *   <li>Given {@link LineTokenizer} (default constructor) IncludeDelims is {@code true}.</li>
   *   <li>Then return {@code foo}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LineTokenizer#getToken(Reader)}
   */
  @Test
  public void testGetToken_givenLineTokenizerIncludeDelimsIsTrue_thenReturnFoo() throws IOException {
    // Arrange
    LineTokenizer lineTokenizer = new LineTokenizer();
    lineTokenizer.setIncludeDelims(true);

    // Act and Assert
    assertEquals("foo", lineTokenizer.getToken(new StringReader("foo")));
  }

  /**
   * Test {@link LineTokenizer#getToken(Reader)}.
   * <ul>
   *   <li>Given {@link LineTokenizer} (default constructor).</li>
   *   <li>When {@link StringReader#StringReader(String)} with empty string.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LineTokenizer#getToken(Reader)}
   */
  @Test
  public void testGetToken_givenLineTokenizer_whenStringReaderWithEmptyString_thenReturnNull() throws IOException {
    // Arrange
    LineTokenizer lineTokenizer = new LineTokenizer();

    // Act and Assert
    assertNull(lineTokenizer.getToken(new StringReader("")));
  }

  /**
   * Test {@link LineTokenizer#getToken(Reader)}.
   * <ul>
   *   <li>Given {@link LineTokenizer} (default constructor).</li>
   *   <li>When {@link StringReader#StringReader(String)} with {@code foo}.</li>
   *   <li>Then return {@code foo}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LineTokenizer#getToken(Reader)}
   */
  @Test
  public void testGetToken_givenLineTokenizer_whenStringReaderWithFoo_thenReturnFoo() throws IOException {
    // Arrange
    LineTokenizer lineTokenizer = new LineTokenizer();

    // Act and Assert
    assertEquals("foo", lineTokenizer.getToken(new StringReader("foo")));
  }

  /**
   * Test {@link LineTokenizer#getPostToken()}.
   * <ul>
   *   <li>Given {@link LineTokenizer} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link LineTokenizer#getPostToken()}
   */
  @Test
  public void testGetPostToken_givenLineTokenizer() {
    // Arrange, Act and Assert
    assertEquals("", (new LineTokenizer()).getPostToken());
  }

  /**
   * Test {@link LineTokenizer#getPostToken()}.
   * <ul>
   *   <li>Given {@link LineTokenizer} (default constructor) IncludeDelims is {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LineTokenizer#getPostToken()}
   */
  @Test
  public void testGetPostToken_givenLineTokenizerIncludeDelimsIsTrue() {
    // Arrange
    LineTokenizer lineTokenizer = new LineTokenizer();
    lineTokenizer.setIncludeDelims(true);

    // Act and Assert
    assertEquals("", lineTokenizer.getPostToken());
  }

  /**
   * Test new {@link LineTokenizer} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link LineTokenizer}
   */
  @Test
  public void testNewLineTokenizer() {
    // Arrange and Act
    LineTokenizer actualLineTokenizer = new LineTokenizer();

    // Assert
    assertEquals("", actualLineTokenizer.getPostToken());
    Location location = actualLineTokenizer.getLocation();
    assertNull(location.getFileName());
    assertNull(actualLineTokenizer.getDescription());
    assertNull(actualLineTokenizer.getProject());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
  }
}
