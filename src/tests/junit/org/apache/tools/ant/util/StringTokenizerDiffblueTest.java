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
  * Methods under test: 
  * 
  * <ul>
  *   <li>default or parameterless constructor of {@link StringTokenizer}
  *   <li>{@link StringTokenizer#setDelimsAreTokens(boolean)}
  *   <li>{@link StringTokenizer#setIncludeDelims(boolean)}
  *   <li>{@link StringTokenizer#setSuppressDelims(boolean)}
  * </ul>
  */
  @Test
  public void testConstructor() {
    // Arrange and Act
    StringTokenizer actualStringTokenizer = new StringTokenizer();
    actualStringTokenizer.setDelimsAreTokens(true);
    actualStringTokenizer.setIncludeDelims(true);
    actualStringTokenizer.setSuppressDelims(true);

    // Assert
    assertNull(actualStringTokenizer.getDescription());
    assertNull(actualStringTokenizer.getProject());
    assertEquals("", actualStringTokenizer.getPostToken());
    Location location = actualStringTokenizer.getLocation();
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertNull(location.getFileName());
  }

  /**
   * Method under test: default or parameterless constructor of {@link StringTokenizer}
   */
  @Test
  public void testConstructor2() {
    // Arrange, Act and Assert
    assertEquals("", (new StringTokenizer()).getPostToken());
  }

  /**
   * Method under test: {@link StringTokenizer#getPostToken()}
   */
  @Test
  public void testGetPostToken() {
    // Arrange, Act and Assert
    assertEquals("", (new StringTokenizer()).getPostToken());
  }

  /**
   * Method under test: {@link StringTokenizer#getPostToken()}
   */
  @Test
  public void testGetPostToken2() {
    // Arrange
    StringTokenizer stringTokenizer = new StringTokenizer();
    stringTokenizer.setSuppressDelims(true);

    // Act and Assert
    assertEquals("", stringTokenizer.getPostToken());
  }

  /**
   * Method under test: {@link StringTokenizer#getPostToken()}
   */
  @Test
  public void testGetPostToken3() {
    // Arrange
    StringTokenizer stringTokenizer = new StringTokenizer();
    stringTokenizer.setIncludeDelims(true);

    // Act and Assert
    assertEquals("", stringTokenizer.getPostToken());
  }

  /**
   * Method under test: {@link StringTokenizer#getToken(Reader)}
   */
  @Test
  public void testGetToken() throws IOException {
    // Arrange
    StringTokenizer stringTokenizer = new StringTokenizer();

    // Act and Assert
    assertEquals("foo", stringTokenizer.getToken(new StringReader("foo")));
    assertEquals("", stringTokenizer.getPostToken());
  }

  /**
   * Method under test: {@link StringTokenizer#getToken(Reader)}
   */
  @Test
  public void testGetToken2() throws IOException {
    // Arrange
    StringTokenizer stringTokenizer = new StringTokenizer();

    // Act and Assert
    assertNull(stringTokenizer.getToken(new StringReader("")));
  }

  /**
   * Method under test: {@link StringTokenizer#getToken(Reader)}
   */
  @Test
  public void testGetToken3() throws IOException {
    // Arrange
    StringTokenizer stringTokenizer = new StringTokenizer();
    stringTokenizer.setDelims("Delims");

    // Act and Assert
    assertEquals("foo", stringTokenizer.getToken(new StringReader("foo")));
    assertEquals("", stringTokenizer.getPostToken());
  }

  /**
   * Method under test: {@link StringTokenizer#getToken(Reader)}
   */
  @Test
  public void testGetToken4() throws IOException {
    // Arrange
    StringTokenizer stringTokenizer = new StringTokenizer();
    stringTokenizer.setIncludeDelims(true);

    // Act and Assert
    assertEquals("foo", stringTokenizer.getToken(new StringReader("foo")));
  }
}

