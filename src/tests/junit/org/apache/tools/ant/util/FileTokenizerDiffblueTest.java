package org.apache.tools.ant.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import org.apache.tools.ant.Location;
import org.junit.Test;

public class FileTokenizerDiffblueTest {
  /**
   * Test {@link FileTokenizer#getToken(Reader)}.
   * <ul>
   *   <li>When {@link StringReader#StringReader(String)} with empty string.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileTokenizer#getToken(Reader)}
   */
  @Test
  public void testGetToken_whenStringReaderWithEmptyString_thenReturnNull() throws IOException {
    // Arrange
    FileTokenizer fileTokenizer = new FileTokenizer();

    // Act and Assert
    assertNull(fileTokenizer.getToken(new StringReader("")));
  }

  /**
   * Test {@link FileTokenizer#getToken(Reader)}.
   * <ul>
   *   <li>When {@link StringReader#StringReader(String)} with {@code foo}.</li>
   *   <li>Then return {@code foo}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileTokenizer#getToken(Reader)}
   */
  @Test
  public void testGetToken_whenStringReaderWithFoo_thenReturnFoo() throws IOException {
    // Arrange
    FileTokenizer fileTokenizer = new FileTokenizer();

    // Act and Assert
    assertEquals("foo", fileTokenizer.getToken(new StringReader("foo")));
  }

  /**
   * Test {@link FileTokenizer#getPostToken()}.
   * <p>
   * Method under test: {@link FileTokenizer#getPostToken()}
   */
  @Test
  public void testGetPostToken() {
    // Arrange, Act and Assert
    assertEquals("", (new FileTokenizer()).getPostToken());
  }

  /**
   * Test new {@link FileTokenizer} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link FileTokenizer}
   */
  @Test
  public void testNewFileTokenizer() {
    // Arrange and Act
    FileTokenizer actualFileTokenizer = new FileTokenizer();

    // Assert
    assertEquals("", actualFileTokenizer.getPostToken());
    Location location = actualFileTokenizer.getLocation();
    assertNull(location.getFileName());
    assertNull(actualFileTokenizer.getDescription());
    assertNull(actualFileTokenizer.getProject());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
  }
}
