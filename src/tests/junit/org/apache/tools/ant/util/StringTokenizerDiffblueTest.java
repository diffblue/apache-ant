package org.apache.tools.ant.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import org.apache.tools.ant.Location;
import org.junit.Test;

public class StringTokenizerDiffblueTest {
  /**
   * Test {@link StringTokenizer#getToken(Reader)}.
   * <ul>
   *   <li>Given {@link StringTokenizer} (default constructor) Delims is {@code Delims}.</li>
   *   <li>Then return {@code foo}.</li>
   * </ul>
   * <p>
   * Method under test: {@link StringTokenizer#getToken(Reader)}
   */
  @Test
  public void testGetToken_givenStringTokenizerDelimsIsDelims_thenReturnFoo() throws IOException {
    // Arrange
    StringTokenizer stringTokenizer = new StringTokenizer();
    stringTokenizer.setDelims("Delims");

    // Act and Assert
    assertEquals("foo", stringTokenizer.getToken(new StringReader("foo")));
  }

  /**
   * Test {@link StringTokenizer#getToken(Reader)}.
   * <ul>
   *   <li>Given {@link StringTokenizer} (default constructor) IncludeDelims is {@code true}.</li>
   *   <li>Then return {@code foo}.</li>
   * </ul>
   * <p>
   * Method under test: {@link StringTokenizer#getToken(Reader)}
   */
  @Test
  public void testGetToken_givenStringTokenizerIncludeDelimsIsTrue_thenReturnFoo() throws IOException {
    // Arrange
    StringTokenizer stringTokenizer = new StringTokenizer();
    stringTokenizer.setIncludeDelims(true);

    // Act and Assert
    assertEquals("foo", stringTokenizer.getToken(new StringReader("foo")));
  }

  /**
   * Test {@link StringTokenizer#getToken(Reader)}.
   * <ul>
   *   <li>Given {@link StringTokenizer} (default constructor).</li>
   *   <li>When {@link StringReader#StringReader(String)} with empty string.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link StringTokenizer#getToken(Reader)}
   */
  @Test
  public void testGetToken_givenStringTokenizer_whenStringReaderWithEmptyString_thenReturnNull() throws IOException {
    // Arrange
    StringTokenizer stringTokenizer = new StringTokenizer();

    // Act and Assert
    assertNull(stringTokenizer.getToken(new StringReader("")));
  }

  /**
   * Test {@link StringTokenizer#getToken(Reader)}.
   * <ul>
   *   <li>Given {@link StringTokenizer} (default constructor).</li>
   *   <li>When {@link StringReader#StringReader(String)} with {@code foo}.</li>
   *   <li>Then return {@code foo}.</li>
   * </ul>
   * <p>
   * Method under test: {@link StringTokenizer#getToken(Reader)}
   */
  @Test
  public void testGetToken_givenStringTokenizer_whenStringReaderWithFoo_thenReturnFoo() throws IOException {
    // Arrange
    StringTokenizer stringTokenizer = new StringTokenizer();

    // Act and Assert
    assertEquals("foo", stringTokenizer.getToken(new StringReader("foo")));
  }

  /**
   * Test {@link StringTokenizer#getPostToken()}.
   * <ul>
   *   <li>Given {@link StringTokenizer} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link StringTokenizer#getPostToken()}
   */
  @Test
  public void testGetPostToken_givenStringTokenizer() {
    // Arrange, Act and Assert
    assertEquals("", (new StringTokenizer()).getPostToken());
  }

  /**
   * Test {@link StringTokenizer#getPostToken()}.
   * <ul>
   *   <li>Given {@link StringTokenizer} (default constructor) IncludeDelims is {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link StringTokenizer#getPostToken()}
   */
  @Test
  public void testGetPostToken_givenStringTokenizerIncludeDelimsIsTrue() {
    // Arrange
    StringTokenizer stringTokenizer = new StringTokenizer();
    stringTokenizer.setIncludeDelims(true);

    // Act and Assert
    assertEquals("", stringTokenizer.getPostToken());
  }

  /**
   * Test {@link StringTokenizer#getPostToken()}.
   * <ul>
   *   <li>Given {@link StringTokenizer} (default constructor) SuppressDelims is {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link StringTokenizer#getPostToken()}
   */
  @Test
  public void testGetPostToken_givenStringTokenizerSuppressDelimsIsTrue() {
    // Arrange
    StringTokenizer stringTokenizer = new StringTokenizer();
    stringTokenizer.setSuppressDelims(true);

    // Act and Assert
    assertEquals("", stringTokenizer.getPostToken());
  }

  /**
   * Test new {@link StringTokenizer} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link StringTokenizer}
   */
  @Test
  public void testNewStringTokenizer() {
    // Arrange and Act
    StringTokenizer actualStringTokenizer = new StringTokenizer();

    // Assert
    assertEquals("", actualStringTokenizer.getPostToken());
    Location location = actualStringTokenizer.getLocation();
    assertNull(location.getFileName());
    assertNull(actualStringTokenizer.getDescription());
    assertNull(actualStringTokenizer.getProject());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
  }
}
