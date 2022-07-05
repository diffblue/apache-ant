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
  * Methods under test: 
  * 
  * <ul>
  *   <li>default or parameterless constructor of {@link LineTokenizer}
  *   <li>{@link LineTokenizer#setIncludeDelims(boolean)}
  * </ul>
  */
  @Test
  public void testConstructor() {
    // Arrange and Act
    LineTokenizer actualLineTokenizer = new LineTokenizer();
    actualLineTokenizer.setIncludeDelims(true);

    // Assert
    assertNull(actualLineTokenizer.getDescription());
    assertNull(actualLineTokenizer.getProject());
    assertEquals("", actualLineTokenizer.getPostToken());
    Location location = actualLineTokenizer.getLocation();
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertNull(location.getFileName());
  }

  /**
   * Method under test: default or parameterless constructor of {@link LineTokenizer}
   */
  @Test
  public void testConstructor2() {
    // Arrange, Act and Assert
    assertEquals("", (new LineTokenizer()).getPostToken());
  }

  /**
   * Method under test: {@link LineTokenizer#getPostToken()}
   */
  @Test
  public void testGetPostToken() {
    // Arrange, Act and Assert
    assertEquals("", (new LineTokenizer()).getPostToken());
  }

  /**
   * Method under test: {@link LineTokenizer#getPostToken()}
   */
  @Test
  public void testGetPostToken2() {
    // Arrange
    LineTokenizer lineTokenizer = new LineTokenizer();
    lineTokenizer.setIncludeDelims(true);

    // Act and Assert
    assertEquals("", lineTokenizer.getPostToken());
  }

  /**
   * Method under test: {@link LineTokenizer#getToken(Reader)}
   */
  @Test
  public void testGetToken() throws IOException {
    // Arrange
    LineTokenizer lineTokenizer = new LineTokenizer();

    // Act and Assert
    assertEquals("foo", lineTokenizer.getToken(new StringReader("foo")));
    assertEquals("", lineTokenizer.getPostToken());
  }

  /**
   * Method under test: {@link LineTokenizer#getToken(Reader)}
   */
  @Test
  public void testGetToken2() throws IOException {
    // Arrange
    LineTokenizer lineTokenizer = new LineTokenizer();

    // Act and Assert
    assertNull(lineTokenizer.getToken(new StringReader("")));
  }

  /**
   * Method under test: {@link LineTokenizer#getToken(Reader)}
   */
  @Test
  public void testGetToken3() throws IOException {
    // Arrange
    LineTokenizer lineTokenizer = new LineTokenizer();
    lineTokenizer.setIncludeDelims(true);

    // Act and Assert
    assertEquals("foo", lineTokenizer.getToken(new StringReader("foo")));
  }
}

