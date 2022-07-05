package org.apache.tools.ant.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import org.junit.Test;

public class FileTokenizerDiffblueTest {
  /**
  * Methods under test: 
  * 
  * <ul>
  *   <li>default or parameterless constructor of {@link FileTokenizer}
  *   <li>{@link FileTokenizer#getPostToken()}
  * </ul>
  */
  @Test
  public void testConstructor() {
    // Arrange, Act and Assert
    assertEquals("", (new FileTokenizer()).getPostToken());
  }

  /**
   * Method under test: default or parameterless constructor of {@link FileTokenizer}
   */
  @Test
  public void testConstructor2() {
    // Arrange and Act
    FileTokenizer actualFileTokenizer = new FileTokenizer();

    // Assert
    assertNull(actualFileTokenizer.getDescription());
    assertNull(actualFileTokenizer.getProject());
    assertEquals("", actualFileTokenizer.getPostToken());
  }

  /**
   * Method under test: {@link FileTokenizer#getToken(Reader)}
   */
  @Test
  public void testGetToken() throws IOException {
    // Arrange
    FileTokenizer fileTokenizer = new FileTokenizer();

    // Act and Assert
    assertEquals("foo", fileTokenizer.getToken(new StringReader("foo")));
  }

  /**
   * Method under test: {@link FileTokenizer#getToken(Reader)}
   */
  @Test
  public void testGetToken2() throws IOException {
    // Arrange
    FileTokenizer fileTokenizer = new FileTokenizer();

    // Act and Assert
    assertNull(fileTokenizer.getToken(new StringReader("")));
  }
}

